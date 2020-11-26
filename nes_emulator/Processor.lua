INSTRUCTION_c = {};
INSTRUCTION_c.__index = INSTRUCTION_c;

function INSTRUCTION(name, operate, addrmode, cycles)
	local t = setmetatable({}, INSTRUCTION_c);

	t.name = name;
	t.operate = operate;
	t.addrmode = addrmode;
	t.cycles = cycles;

	return t;
end

Processor = {};
Processor.__index = Processor;

Processor.FLAGS6502 = {
	C = bitLShift(1, 0), -- Carry Bit
	Z = bitLShift(1, 1), -- Zero
	I = bitLShift(1, 2), -- Disble Interrupts
	D = bitLShift(1, 3), -- Decimal Mode (Unused)
	B = bitLShift(1, 4), -- Break
	U = bitLShift(1, 5), -- Unused
	V = bitLShift(1, 6), -- Overflow
	N = bitLShift(1, 7), -- Negative
}

function Processor:Create()
	local t = setmetatable({}, Processor);

	t.bus = nil;

	-- Registers
	t.a = 0x00; -- Accumulator
	t.x = 0x00; -- X
	t.y = 0x00; -- Y
	t.stkp = 0x00; -- Stack Pointer (location on bus)
	t.pc = 0x0000; -- Program Counter
	t.status = 0x00; -- Status register



	t.IMP = function()
		t.fetched = t.a;
		return 0;
	end;

	t.IMM = function()
		t.addr_abs = t.pc;
		t.pc = t.pc+1;
		return 0;
	end;

	t.ZP0 = function()
		t.addr_abs = t:Read(t.pc);
		t.pc = t.pc+1;
		t.addr_abs = bitAnd(t.addr_abs, 0x00FF);

		return 0;
	end;

	t.ZPX = function()
		t.addr_abs = (t:Read(t.pc)+t.x);
		t.pc = t.pc+1;
		t.addr_abs = bitAnd(t.addr_abs, 0x00FF);
		return 0;
	end;

	t.ZPY = function()
		t.addr_abs = (t:Read(t.pc)+t.y);
		t.pc = t.pc+1;
		t.addr_abs = bitAnd(t.addr_abs, 0x00FF);
		return 0;
	end;

	t.ABS = function()
		local lo = t:Read(t.pc);
		t.pc = t.pc+1;
		local hi = t:Read(t.pc);
		t.pc = t.pc+1;

		t.addr_abs = bitOr(bitLShift(hi, 8), lo);

		return 0;
	end;

	t.ABX = function()
		local lo = t:Read(t.pc);
		t.pc = t.pc+1;
		local hi = t:Read(t.pc);
		t.pc = t.pc+1;

		t.addr_abs = bitOr(bitLShift(hi, 8), lo);
		t.addr_abs = t.addr_abs+t.x;
		if((bitAnd(t.addr_abs, 0xFF00)~=bitLShift(hi, 8)))then
			return 1;
		else
			return 0;
		end
	end;

	t.ABY = function()
		local lo = t:Read(t.pc);
		t.pc = t.pc+1;
		local hi = t:Read(t.pc);
		t.pc = t.pc+1;

		t.addr_abs = bitOr(bitLShift(hi, 8), lo);
		t.addr_abs = t.addr_abs+t.y;
		if((bitAnd(t.addr_abs, 0xFF00)~=bitLShift(hi, 8)))then
			return 1;
		else
			return 0;
		end
	end;

	t.IND = function()
		local ptr_lo = t:Read(t.pc);
		t.pc = t.pc+1;
		local ptr_hi = t:Read(t.pc);
		t.pc = t.pc+1;

		local ptr = bitOr(bitLShift(ptr_hi, 8), ptr_lo);

		if(ptr_lo==0x00FF)then
			t.addr_abs = (bitOr(bitLShift(t:Read(bitAnd(ptr, 0xFF00)), 8), t:Read(ptr+0)));
		else
			t.addr_abs = (bitOr(bitLShift(t:Read(ptr+1), 8), t:Read(ptr+0)));
		end

		return 0;
	end;

	t.IZX = function()
		local _t = t:Read(t.pc);
		t.pc = t.pc+1;

		local lo = t:Read(bitAnd(_t+t.x, 0x00FF));
		local hi = t:Read(bitAnd(_t+t.x+1, 0x00FF));

		t.addr_abs = bitOr(bitAnd(hi, 8), lo);

		return 0;
	end;

	t.IZY = function()
		local _t = t:Read(t.pc);
		t.pc = t.pc+1;

		local lo = t:Read(bitAnd(_t, 0x00FF));
		local hi = t:Read(bitAnd(_t+1, 0x00FF));

		t.addr_abs = bitOr(bitAnd(hi, 8), lo);
		t.addr_abs = t.addr_abs+t.y;
		if(bitAnd(t.addr_abs, 0xFF00)~=bitLShift(hi, 8))then
			return 1;
		else
			return 0;
		end
	end;

	t.REL = function()
		t.addr_rel = t:Read(t.pc);
		t.pc = t.pc+1;

		if(bitAnd(t.addr_rel, 0x80)==1)then -- CHECK THIS WHEN ALL FAILS, UNKNOWN WHETHER THIS IS CORRECT WAY TO CONDITION BITWISE AND
			t.addr_rel = bitOr(t.addr_rel, 0xFF00);
		end
		return 0;
	end;


	t.AND = function()
		t:Fetch();

		t.a = bitAnd(t.a, t.fetched);
		t:SetFlag(Processor.FLAGS6502.Z, t.a==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.a, 0x80));

		return 1;
	end;

	t.ASL = function() 
		t:Fetch();
		local temp = bitLShift(t.fetched, 1);
		t:SetFlag(Processor.FLAGS6502.C, bitAnd(temp, 0xFF00)>0);
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0xFF00)==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x80));

		if(t.lookup[t.opcode+1].addrmode==t.IMP)then
			t.a = bitAnd(temp, 0x00FF);
		else
			t:Write(t.addr_abs, bitAnd(temp, 0x00FF));
		end
		return 0;
	end;

	t.BCS = function()
		if(t:GetFlag(Processor.FLAGS6502.C)==1)then
			t.cycles = t.cycles+1;
			t.addr_abs = t.pc+t.addr_rel;

			if(bitAnd(t.addr_abs, 0xFF00) ~= bitAnd(t.pc, 0xFF00))then
				t.cycles = t.cycles+1;
			end

			t.pc = t.addr_abs;
		end
		return 1;
	end;

	t.BCC = function()
		if(t:GetFlag(Processor.FLAGS6502.C)==1)then
			t.cycles = t.cycles+1;
			t.addr_abs = t.pc+t.addr_rel;

			if(bitAnd(t.addr_abs, 0xFF00) ~= bitAnd(t.pc, 0xFF00))then
				t.cycles = t.cycles+1;
			end

			t.pc = t.addr_abs;
		end
		return 1;
	end;

	t.BEQ = function()
		if(t:GetFlag(Processor.FLAGS6502.Z)==1)then
			t.cycles = t.cycles+1;
			t.addr_abs = t.pc+t.addr_rel;

			if(bitAnd(t.addr_abs, 0xFF00) ~= bitAnd(t.pc, 0xFF00))then
				t.cycles = t.cycles+1;
			end

			t.pc = t.addr_abs;
		end
		return 1;
	end;

	t.BMI = function()
		if(t:GetFlag(Processor.FLAGS6502.N)==1)then
			t.cycles = t.cycles+1;
			t.addr_abs = t.pc+t.addr_rel;

			if(bitAnd(t.addr_abs, 0xFF00) ~= bitAnd(t.pc, 0xFF00))then
				t.cycles = t.cycles+1;
			end

			t.pc = t.addr_abs;
		end
		return 1;
	end;

	t.BNE = function()
		if(t:GetFlag(Processor.FLAGS6502.Z)==1)then
			t.cycles = t.cycles+1;
			t.addr_abs = t.pc+t.addr_rel;

			if(bitAnd(t.addr_abs, 0xFF00) ~= bitAnd(t.pc, 0xFF00))then
				t.cycles = t.cycles+1;
			end

			t.pc = t.addr_abs;
		end
		return 1;
	end;

	t.BPL = function()
		if(t:GetFlag(Processor.FLAGS6502.N)==1)then
			t.cycles = t.cycles+1;
			t.addr_abs = t.pc+t.addr_rel;

			if(bitAnd(t.addr_abs, 0xFF00) ~= bitAnd(t.pc, 0xFF00))then
				t.cycles = t.cycles+1;
			end

			t.pc = t.addr_abs;
		end
		return 1;
	end;

	t.BVC = function()
		if(t:GetFlag(Processor.FLAGS6502.V)==1)then
			t.cycles = t.cycles+1;
			t.addr_abs = t.pc+t.addr_rel;

			if(bitAnd(t.addr_abs, 0xFF00) ~= bitAnd(t.pc, 0xFF00))then
				t.cycles = t.cycles+1;
			end

			t.pc = t.addr_abs;
		end
		return 1;
	end;

	t.BVS = function()
		if(t:GetFlag(Processor.FLAGS6502.V)==1)then
			t.cycles = t.cycles+1;
			t.addr_abs = t.pc+t.addr_rel;

			if(bitAnd(t.addr_abs, 0xFF00) ~= bitAnd(t.pc, 0xFF00))then
				t.cycles = t.cycles+1;
			end

			t.pc = t.addr_abs;
		end
		return 1;
	end;

	t.CLC = function()
		t:SetFlag(Processor.FLAGS6502.C, false);
		return 0;
	end;

	t.CLD = function()
		t:SetFlag(Processor.FLAGS6502.D, false);
		return 0;
	end;

	t.CLI = function()
		t:SetFlag(Processor.FLAGS6502.I, false);
		return 0;
	end;

	t.CLV = function()
		t:SetFlag(Processor.FLAGS6502.V, false);
		return 0;
	end;

	t.CMP = function()
		t:Fetch();

		local temp = t.a-t.fetched;
		t:SetFlag(Processor.FLAGS6502.C, t.a>=t.fetched);
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0x0000);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x0080));

		return 1;
	end;

	t.ADC = function()
		t:Fetch();

		local temp = t.a+t.fetched+t:GetFlag(Processor.FLAGS6502.C);
		t:SetFlag(Processor.FLAGS6502.C, temp>255);
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0);
		t:SetFlag(Processor.FLAGS6502.V, bitAnd(bitNot(bitXor(t.a, t.fetched)), bitXor(t.a, temp), 0x0080));
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x80));
		t.a = bitAnd(temp, 0x00FF);
		return 1;
	end;

	t.SBC = function()
		t:Fetch();

		local value = bitXor(t.fetched, 0x00FF);

		local temp = t.a+value+t:GetFlag(Processor.FLAGS6502.C);
		t:SetFlag(Processor.FLAGS6502.C, bitAnd(temp, 0xFF00));
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0);
		t:SetFlag(Processor.FLAGS6502.V, bitAnd(bitXor(temp, t.a), bitXor(temp, value), 0x0080));
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x80));
		t.a = bitAnd(temp, 0x00FF);
		return 1;
	end;

	t.PHA = function()
		t:Write(0x0100+t.stkp, t.a); -- Hardcoded base location based on the 6502 processor, using stack pointer as offset
		t.stkp = t.stkp-1;

		return 0;
	end

	t.PLA = function()
		t.stkp = t.stkp+1;
		t.a = t:Read(0x0100+t.stkp);
		t:SetFlag(Processor.FLAGS6502.Z, t.a==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(a, 0x80));
		return 0;
	end

	t.RTI = function()
		t.stkp = t.stkp+1;
		t.status = t:Read(0x0100+t.stkp);
		t.status = bitAnd(t.status, bitNot(Processor.FLAGS6502.B));
		t.status = bitAnd(t.status, bitNot(Processor.FLAGS6502.U));

		t.stkp = t.stkp+1;
		t.pc = t:Read(0x0100+t.stkp);
		t.stkp = t.stkp+1;
		t.pc = bitOr(t.pc, bitLShift(t:Read(0x0100+t.stkp),8));
		return 0;
	end;	

	t.BIT = function()
		t:Fetch();

		local temp = bitAnd(t.a, t.fetched);
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, bitLShift(1, 7)));
		t:SetFlag(Processor.FLAGS6502.V, bitAnd(temp, bitLShift(1, 6)));
		return 0;
	end;

	t.BRK = function()
		t.pc = t.pc+1;

		t:SetFlag(Processor.FLAGS6502.I, 1);
		t:Write(0x0100 + t.stkp, bitAnd(bitRShift(t.pc,8), 0x00FF));
		t.stkp = t.stkp-1;
		t:Write(0x0100 + t.stkp, bitAnd(t.pc,0x00FF));
		t.stkp = t.stkp-1;

		t:SetFlag(Processor.FLAGS6502.B, 1);
		t:Write(0x0100 + t.stkp, t.status);
		t.stkp = t.stkp-1;
		t:SetFlag(Processor.FLAGS6502.B, 0);

		t.pc = bitOr(t:Read(0xFFFE), bitLShift(t:Read(0xFFFF), 8));
		return 0;
	end;

	t.CPX = function()
		t:Fetch();
		local temp = t.x-t.fetched;
		t:SetFlag(Processor.FLAGS6502.C, t.x>=t.fetched);
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0x0000);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x0080));
		return 0;
	end;

	t.CPY = function()
		t:Fetch();
		local temp = t.y-t.fetched;
		t:SetFlag(Processor.FLAGS6502.C, t.y>=t.fetched);
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0x0000);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x0080));
		return 0;
	end;

	t.DEC = function()
		t:Fetch();
		local temp = t.fetched-1;
		t:Write(t.addr_abs, bitAnd(temp, 0x00FF));
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0x0000);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x0080));
		return 0;
	end;

	t.DEX = function()
		t.x = t.x-1;
		t:SetFlag(Processor.FLAGS6502.Z, t.x==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.x, 0x80));
		return 0;
	end;

	t.DEX = function()
		t.y = t.y-1;
		t:SetFlag(Processor.FLAGS6502.Z, t.y==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.y, 0x80));
		return 0;
	end;

	t.EOR = function()
		t:Fetch();
		t.a = bitXor(t.a, t.fetched);
		t:SetFlag(Processor.FLAGS6502.Z, t.a==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.a, 0x80));
		return 1;
	end;

	t.INC = function()
		t:Fetch();
		local temp = t.fetched+1;
		t:Write(t.addr_abs, bitAnd(temp, 0x00FF));
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0x0000);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x0080));
		return 0;
	end;

	t.INX = function()
		t.x = t.x+1;
		t:SetFlag(Processor.FLAGS6502.Z, t.x==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.x,0x80));
		return 0;
	end;	

	t.INY = function()
		t.y = t.y+1;
		t:SetFlag(Processor.FLAGS6502.Z, t.y==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.y,0x80));
		return 0;
	end;	

	t.JMP = function()
		t.pc = t.addr_abs;
		return 0;
	end;

	t.JSR = function()
		t.pc = t.pc-1;

		t:Write(0x0100+t.stkp, bitAnd(bitRShift(t.pc, 8), 0x00FF));
		t.stkp = t.stkp-1;
		t:Write(0x0100+t.stkp, bitAnd(t.pc, 0x00FF));
		t.stkp = t.stkp-1;

		t.pc = t.addr_abs;
		return 0;
	end;	

	t.LDA = function()
		t:Fetch();

		t.a = t.fetched;
		t:SetFlag(Processor.FLAGS6502.Z, t.a==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.a,0x80));
		return 1;
	end;	
	t.LDX = function()
		t:Fetch();

		t.x = t.fetched;
		t:SetFlag(Processor.FLAGS6502.Z, t.x==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.x,0x80));
		return 1;
	end;	
	t.LDY = function()
		t:Fetch();

		t.y = t.fetched;
		t:SetFlag(Processor.FLAGS6502.Z, t.y==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.y,0x80));
		return 1;
	end;

	t.LSR = function()
		t:Fetch();

		t:SetFlag(Processor.FLAGS6502.C, bitAnd(t.fetched, 0x0001));
		local temp = bitRShift(t.fetched, 1);
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0x0000);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x0080));

		if(t.lookup[t.opcode+1].addrmode==t.IMP)then
			t.a = bitAnd(t.temp, 0x00FF);
		else
			t:Write(t.addr_abs, bitAnd(temp, 0x00FF));
		end
		return 0;
	end;

	t.NOP = function()
		if(t.opcode==0x1C or
			t.opcode==0x3C or
			t.opcode==0x5C or
			t.opcode==0x7C or
			t.opcode==0xDC or
			t.opcode==0xFC)then
			print("Illegal");
			return 1;
		end
		return 0;
	end;

	t.ORA = function()
		t:Fetch();
		t.a = bitOr(t.a, t.fetched);
		t:SetFlag(Processor.FLAGS6502.Z, t.a==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.a,0x00));
		return 1;
	end;	

	t.PHP = function()
		t:Write(0x100+t.stkp, bitOr(t.status, Processor.FLAGS6502.B, Processor.FLAGS6502.U));
		t:SetFlag(Processor.FLAGS6502.B, 0);
		t:SetFlag(Processor.FLAGS6502.U, 0);
		t.stkp = t.stkp-1;
		return 0;
	end;	

	t.PLP = function()
		t.stkp = t.stkp+1;
		t.status = t:Read(0x0100+t.stkp);
		t:SetFlag(Processor.FLAGS6502.U, 1);
		return 0;
	end;

	t.ROL = function()
		t:Fetch();

		local temp = bitOr(bitLShift(t.fetched, 1), t:GetFlag(Processor.FLAGS6502.C));
		t:SetFlag(Processor.FLAGS6502.C, bitAnd(temp, 0xFF00));
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0x0000);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x0080));

		if(t.lookup[t.opcode+1].addrmode==t.IMP)then
			t.a = bitAnd(temp, 0x00FF);
		else
			t:Write(t.addr_abs, bitAnd(temp, 0x00FF));
		end
		return 0;
	end;

	t.ROR = function()
		t:Fetch();

		local temp = bitOr(bitLShift(t.fetched, 7), bitRShift(t.fetched, 1));
		t:SetFlag(Processor.FLAGS6502.C, bitAnd(temp, 0x01));
		t:SetFlag(Processor.FLAGS6502.Z, bitAnd(temp, 0x00FF)==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(temp, 0x0080));

		if(t.lookup[t.opcode+1].addrmode==t.IMP)then
			t.a = bitAnd(temp, 0x00FF);
		else
			t:Write(t.addr_abs, bitAnd(temp, 0x00FF));
		end
		return 0;
	end;
	
	t.RTS = function()
		t.stkp = t.stkp+1;
		t.pc = t:Read(0x0100+t.stkp);
		t.stkp = t.stkp+1;
		t.pc = bitOr(t.pc, bitLShift(t:Read(0x0100+t.stkp),8));
		t.pc = t.pc+1;
		return 0;
	end;

	t.SEC = function()
		t:SetFlag(Processor.FLAGS6502.C, true);
		return 0;
	end;
	t.SED = function()
		t:SetFlag(Processor.FLAGS6502.D, true);
		return 0;
	end;	
	t.SEI = function()
		t:SetFlag(Processor.FLAGS6502.I, true);
		return 0;
	end;	

	t.STA = function()
		t:Write(t.addr_abs, t.a);
		return 0;
	end;

	t.STX = function()
		t:Write(t.addr_abs, t.x);
		return 0;
	end;
	
	t.STY = function()
		t:Write(t.addr_abs, t.y);
		return 0;
	end;

	t.TAX = function()
		t.x = t.a;
		t:SetFlag(Processor.FLAGS6502.Z, t.x==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.x, 0x80));
		return 0;
	end;	

	t.TAY = function()
		t.y = t.a;
		t:SetFlag(Processor.FLAGS6502.Z, t.y==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.y, 0x80));
		return 0;
	end;

	t.TSX = function()
		t.x = t.stkp;
		t:SetFlag(Processor.FLAGS6502.Z, t.x==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.x, 0x80));
		return 0;
	end;	
	t.TXA = function()
		t.a = t.stkp;
		t:SetFlag(Processor.FLAGS6502.Z, t.a==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.a, 0x80));
		return 0;
	end;	
	t.TXS = function()
		t.stkp = t.x;
		return 0;
	end;	
	t.TYA = function()
		t.a = t.y;
		t:SetFlag(Processor.FLAGS6502.Z, t.a==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.a, 0x80));
		return 0;
	end;
	t.DEY = function()
		t.y = t.y-1;
		t:SetFlag(Processor.FLAGS6502.Z, t.y==0x00);
		t:SetFlag(Processor.FLAGS6502.N, bitAnd(t.y, 0x80));
		return 0;
	end;

	t.XXX = function() return 0; end;

	t.fetched = 0x00;

	t.addr_abs = 0x0000;
	t.addr_rel = 0x00;
	t.opcode = 0x00;
	t.cycles = 0;

	t.lookup = {
		INSTRUCTION("BRK", t.BRK, t.IMM, 7),INSTRUCTION("ORA", t.ORA, t.IZX, 6),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("???", t.NOP, t.IMP, 3),INSTRUCTION("ORA", t.ORA, t.ZP0, 3),INSTRUCTION("ASL", t.ASL, t.ZP0, 5),INSTRUCTION("???", t.XXX, t.IMP, 5),INSTRUCTION("PHP", t.PHP, t.IMP, 3),INSTRUCTION("ORA", t.ORA, t.IMM, 2),INSTRUCTION("ASL", t.ASL, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("ORA", t.ORA, t.ABS, 4),INSTRUCTION("ASL", t.ASL, t.ABS, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),
		INSTRUCTION("BPL", t.BPL, t.REL, 2),INSTRUCTION("ORA", t.ORA, t.IZY, 5),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("ORA", t.ORA, t.ZPX, 4),INSTRUCTION("ASL", t.ASL, t.ZPX, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),INSTRUCTION("CLC", t.CLC, t.IMP, 2),INSTRUCTION("ORA", t.ORA, t.ABY, 4),INSTRUCTION("???", t.NOP, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 7),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("ORA", t.ORA, t.ABX, 4),INSTRUCTION("ASL", t.ASL, t.ABX, 7),INSTRUCTION("???", t.XXX, t.IMP, 7),
		INSTRUCTION("JSR", t.JSR, t.ABS, 6),INSTRUCTION("AND", t.AND, t.IZX, 6),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("BIT", t.BIT, t.ZP0, 3),INSTRUCTION("AND", t.AND, t.ZP0, 3),INSTRUCTION("ROL", t.ROL, t.ZP0, 5),INSTRUCTION("???", t.XXX, t.IMP, 5),INSTRUCTION("PLP", t.PLP, t.IMP, 4),INSTRUCTION("AND", t.AND, t.IMM, 2),INSTRUCTION("ROL", t.ROL, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("BIT", t.BIT, t.ABS, 4),INSTRUCTION("AND", t.AND, t.ABS, 4),INSTRUCTION("ROL", t.ROL, t.ABS, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),
		INSTRUCTION("BMI", t.BMI, t.REL, 2),INSTRUCTION("AND", t.AND, t.IZY, 5),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("AND", t.AND, t.ZPX, 4),INSTRUCTION("ROL", t.ROL, t.ZPX, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),INSTRUCTION("SEC", t.SEC, t.IMP, 2),INSTRUCTION("AND", t.AND, t.ABY, 4),INSTRUCTION("???", t.NOP, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 7),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("AND", t.AND, t.ABX, 4),INSTRUCTION("ROL", t.ROL, t.ABX, 7),INSTRUCTION("???", t.XXX, t.IMP, 7),
		INSTRUCTION("RTI", t.RTI, t.IMP, 6),INSTRUCTION("EOR", t.EOR, t.IZX, 6),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("???", t.NOP, t.IMP, 3),INSTRUCTION("EOR", t.EOR, t.ZP0, 3),INSTRUCTION("LSR", t.LSR, t.ZP0, 5),INSTRUCTION("???", t.XXX, t.IMP, 5),INSTRUCTION("PHA", t.PHA, t.IMP, 3),INSTRUCTION("EOR", t.EOR, t.IMM, 2),INSTRUCTION("LSR", t.LSR, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("JMP", t.JMP, t.ABS, 3),INSTRUCTION("EOR", t.EOR, t.ABS, 4),INSTRUCTION("LSR", t.LSR, t.ABS, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),
		INSTRUCTION("BVC", t.BVC, t.REL, 2),INSTRUCTION("EOR", t.EOR, t.IZY, 5),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("EOR", t.EOR, t.ZPX, 4),INSTRUCTION("LSR", t.LSR, t.ZPX, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),INSTRUCTION("CLI", t.CLI, t.IMP, 2),INSTRUCTION("EOR", t.EOR, t.ABY, 4),INSTRUCTION("???", t.NOP, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 7),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("EOR", t.EOR, t.ABX, 4),INSTRUCTION("LSR", t.LSR, t.ABX, 7),INSTRUCTION("???", t.XXX, t.IMP, 7),
		INSTRUCTION("RTS", t.RTS, t.IMP, 6),INSTRUCTION("ADC", t.ADC, t.IZX, 6),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("???", t.NOP, t.IMP, 3),INSTRUCTION("ADC", t.ADC, t.ZP0, 3),INSTRUCTION("ROR", t.ROR, t.ZP0, 5),INSTRUCTION("???", t.XXX, t.IMP, 5),INSTRUCTION("PLA", t.PLA, t.IMP, 4),INSTRUCTION("ADC", t.ADC, t.IMM, 2),INSTRUCTION("ROR", t.ROR, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("JMP", t.JMP, t.IND, 5),INSTRUCTION("ADC", t.ADC, t.ABS, 4),INSTRUCTION("ROR", t.ROR, t.ABS, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),
		INSTRUCTION("BVS", t.BVS, t.REL, 2),INSTRUCTION("ADC", t.ADC, t.IZY, 5),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("ADC", t.ADC, t.ZPX, 4),INSTRUCTION("ROR", t.ROR, t.ZPX, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),INSTRUCTION("SEI", t.SEI, t.IMP, 2),INSTRUCTION("ADC", t.ADC, t.ABY, 4),INSTRUCTION("???", t.NOP, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 7),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("ADC", t.ADC, t.ABX, 4),INSTRUCTION("ROR", t.ROR, t.ABX, 7),INSTRUCTION("???", t.XXX, t.IMP, 7),
		INSTRUCTION("???", t.NOP, t.IMP, 2),INSTRUCTION("STA", t.STA, t.IZX, 6),INSTRUCTION("???", t.NOP, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 6),INSTRUCTION("STY", t.STY, t.ZP0, 3),INSTRUCTION("STA", t.STA, t.ZP0, 3),INSTRUCTION("STX", t.STX, t.ZP0, 3),INSTRUCTION("???", t.XXX, t.IMP, 3),INSTRUCTION("DEY", t.DEY, t.IMP, 2),INSTRUCTION("???", t.NOP, t.IMP, 2),INSTRUCTION("TXA", t.TXA, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("STY", t.STY, t.ABS, 4),INSTRUCTION("STA", t.STA, t.ABS, 4),INSTRUCTION("STX", t.STX, t.ABS, 4),INSTRUCTION("???", t.XXX, t.IMP, 4),
		INSTRUCTION("BCC", t.BCC, t.REL, 2),INSTRUCTION("STA", t.STA, t.IZY, 6),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 6),INSTRUCTION("STY", t.STY, t.ZPX, 4),INSTRUCTION("STA", t.STA, t.ZPX, 4),INSTRUCTION("STX", t.STX, t.ZPY, 4),INSTRUCTION("???", t.XXX, t.IMP, 4),INSTRUCTION("TYA", t.TYA, t.IMP, 2),INSTRUCTION("STA", t.STA, t.ABY, 5),INSTRUCTION("TXS", t.TXS, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 5),INSTRUCTION("???", t.NOP, t.IMP, 5),INSTRUCTION("STA", t.STA, t.ABX, 5),INSTRUCTION("???", t.XXX, t.IMP, 5),INSTRUCTION("???", t.XXX, t.IMP, 5),
		INSTRUCTION("LDY", t.LDY, t.IMM, 2),INSTRUCTION("LDA", t.LDA, t.IZX, 6),INSTRUCTION("LDX", t.LDX, t.IMM, 2),INSTRUCTION("???", t.XXX, t.IMP, 6),INSTRUCTION("LDY", t.LDY, t.ZP0, 3),INSTRUCTION("LDA", t.LDA, t.ZP0, 3),INSTRUCTION("LDX", t.LDX, t.ZP0, 3),INSTRUCTION("???", t.XXX, t.IMP, 3),INSTRUCTION("TAY", t.TAY, t.IMP, 2),INSTRUCTION("LDA", t.LDA, t.IMM, 2),INSTRUCTION("TAX", t.TAX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("LDY", t.LDY, t.ABS, 4),INSTRUCTION("LDA", t.LDA, t.ABS, 4),INSTRUCTION("LDX", t.LDX, t.ABS, 4),INSTRUCTION("???", t.XXX, t.IMP, 4),
		INSTRUCTION("BCS", t.BCS, t.REL, 2),INSTRUCTION("LDA", t.LDA, t.IZY, 5),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 5),INSTRUCTION("LDY", t.LDY, t.ZPX, 4),INSTRUCTION("LDA", t.LDA, t.ZPX, 4),INSTRUCTION("LDX", t.LDX, t.ZPY, 4),INSTRUCTION("???", t.XXX, t.IMP, 4),INSTRUCTION("CLV", t.CLV, t.IMP, 2),INSTRUCTION("LDA", t.LDA, t.ABY, 4),INSTRUCTION("TSX", t.TSX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 4),INSTRUCTION("LDY", t.LDY, t.ABX, 4),INSTRUCTION("LDA", t.LDA, t.ABX, 4),INSTRUCTION("LDX", t.LDX, t.ABY, 4),INSTRUCTION("???", t.XXX, t.IMP, 4),
		INSTRUCTION("CPY", t.CPY, t.IMM, 2),INSTRUCTION("CMP", t.CMP, t.IZX, 6),INSTRUCTION("???", t.NOP, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("CPY", t.CPY, t.ZP0, 3),INSTRUCTION("CMP", t.CMP, t.ZP0, 3),INSTRUCTION("DEC", t.DEC, t.ZP0, 5),INSTRUCTION("???", t.XXX, t.IMP, 5),INSTRUCTION("INY", t.INY, t.IMP, 2),INSTRUCTION("CMP", t.CMP, t.IMM, 2),INSTRUCTION("DEX", t.DEX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("CPY", t.CPY, t.ABS, 4),INSTRUCTION("CMP", t.CMP, t.ABS, 4),INSTRUCTION("DEC", t.DEC, t.ABS, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),
		INSTRUCTION("BNE", t.BNE, t.REL, 2),INSTRUCTION("CMP", t.CMP, t.IZY, 5),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("CMP", t.CMP, t.ZPX, 4),INSTRUCTION("DEC", t.DEC, t.ZPX, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),INSTRUCTION("CLD", t.CLD, t.IMP, 2),INSTRUCTION("CMP", t.CMP, t.ABY, 4),INSTRUCTION("NOP", t.NOP, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 7),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("CMP", t.CMP, t.ABX, 4),INSTRUCTION("DEC", t.DEC, t.ABX, 7),INSTRUCTION("???", t.XXX, t.IMP, 7),
		INSTRUCTION("CPX", t.CPX, t.IMM, 2),INSTRUCTION("SBC", t.SBC, t.IZX, 6),INSTRUCTION("???", t.NOP, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("CPX", t.CPX, t.ZP0, 3),INSTRUCTION("SBC", t.SBC, t.ZP0, 3),INSTRUCTION("INC", t.INC, t.ZP0, 5),INSTRUCTION("???", t.XXX, t.IMP, 5),INSTRUCTION("INX", t.INX, t.IMP, 2),INSTRUCTION("SBC", t.SBC, t.IMM, 2),INSTRUCTION("NOP", t.NOP, t.IMP, 2),INSTRUCTION("???", t.SBC, t.IMP, 2),INSTRUCTION("CPX", t.CPX, t.ABS, 4),INSTRUCTION("SBC", t.SBC, t.ABS, 4),INSTRUCTION("INC", t.INC, t.ABS, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),
		INSTRUCTION("BEQ", t.BEQ, t.REL, 2),INSTRUCTION("SBC", t.SBC, t.IZY, 5),INSTRUCTION("???", t.XXX, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 8),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("SBC", t.SBC, t.ZPX, 4),INSTRUCTION("INC", t.INC, t.ZPX, 6),INSTRUCTION("???", t.XXX, t.IMP, 6),INSTRUCTION("SED", t.SED, t.IMP, 2),INSTRUCTION("SBC", t.SBC, t.ABY, 4),INSTRUCTION("NOP", t.NOP, t.IMP, 2),INSTRUCTION("???", t.XXX, t.IMP, 7),INSTRUCTION("???", t.NOP, t.IMP, 4),INSTRUCTION("SBC", t.SBC, t.ABX, 4),INSTRUCTION("INC", t.INC, t.ABX, 7),INSTRUCTION("???", t.XXX, t.IMP, 7),
	};

	return t;
end

function Processor:ConnectBus(bus)
	self.bus = bus;
end

function Processor:Write(addr, data)
	self.bus:Write(addr, data);
end

function Processor:Read(addr)
	return self.bus:Read(addr, true);
end

function Processor:SetFlag(flag, v)
	if(v)then
		self.status = bitOr(self.status, flag);
	else
		self.status = bitAnd(self.status, bitNot(flag));
	end
end

function Processor:GetFlag(flag)
	return ((bitAnd(self.status, flag)>0) and 1 or 0);
end

function Processor:Clock()
	if(self.cycles==0)then
		self.opcode = self:Read(self.pc);
		self.pc = self.pc+1;
		local lookup_res = self.lookup[self.opcode+1];
		print(lookup_res.name.."");
		self.cycles = self.lookup[self.opcode+1].cycles;

		local additional_cycle1 = self.lookup[self.opcode+1].addrmode();
		local additional_cycle2 = self.lookup[self.opcode+1].operate();

		--print(additional_cycle1);

		self.cycles = self.cycles+(bitAnd(additional_cycle1, additional_cycle2));
	end

	self.cycles = self.cycles-1;
end

function Processor:Reset()
	self.a = 0;
	self.x = 0;
	self.y = 0;
	self.stkp = 0xFD;
	self.status = bitOr(0x00, Processor.FLAGS6502.U);

	self.addr_abs = 0xFFFC;
	local lo = self:Read(self.addr_abs+0);
	local hi = self:Read(self.addr_abs+1);

	self.pc = bitOr(bitLShift(hi, 8), lo);

	self.addr_rel = 0x0000;
	self.addr_abs = 0x0000;
	self.fetched = 0x00;

	self.cycles = 8;
end

function Processor:Irq()
	if(self:GetFlag(Processor.FLAGS6502.I)==0)then
		self:Write(0x0100+self.stkp, bitAnd(bitRShift(self.pc, 8), 0x00FF));
		self.stkp = self.stkp-1;
		self:Write(0x0100+self.stkp, bitAnd(self.pc, 0x00FF));
		self.stkp = self.stkp-1;

		self:SetFlag(Processor.FLAGS6502.B, 0);
		self:SetFlag(Processor.FLAGS6502.U, 1);
		self:SetFlag(Processor.FLAGS6502.I, 1);
		self:Write(0x0100+self.stkp, self.status);
		self.stkp = self.stkp-1;

		self.addr_abs = 0xFFFE;
		local lo = self:Read(self.addr_abs+0);
		local hi = self:Read(self.addr_abs+1);
		self.pc = bitOr(bitLShift(hi, 8), lo);

		self.cycles = 7;
	end
end

function Processor:Nmi()
	self:Write(0x0100+self.stkp, bitAnd(bitRShift(self.pc, 8), 0x00FF));
	self.stkp = self.stkp-1;
	self:Write(0x0100+self.stkp, bitAnd(self.pc, 0x00FF));
	self.stkp = self.stkp-1;

	self:SetFlag(Processor.FLAGS6502.B, 0);
	self:SetFlag(Processor.FLAGS6502.U, 1);
	self:SetFlag(Processor.FLAGS6502.I, 1);
	self:Write(0x0100+self.stkp, self.status);
	self.stkp = self.stkp-1;

	self.addr_abs = 0xFFFE;
	local lo = self:Read(self.addr_abs+0);
	local hi = self:Read(self.addr_abs+1);
	self.pc = bitOr(bitLShift(hi, 8), lo);

	self.cycles = 8;
end

function Processor:Fetch()
	if(not(self.lookup[self.opcode+1].addrmode==self.IMP))then
		self.fetched = self:Read(self.addr_abs);
	end
	return self.fetched
end

function Processor:Complete()
	return self.cycles==0;
end

function Processor:Disassemble(nStart, nStop)
	local addr = nStart;
	local value = 0x00;
	local lo = 0x00;
	local hi = 0x00;
	local mapLines = {};
	local line_addr = 0;

	while(addr<=nStop)do
		line_addr = addr;

		local sInst = "$"..hex(addr, 4)..": ";

		local _opcode = self.bus:Read(addr, true);
		addr = addr+1;
		sInst = sInst..(self.lookup[_opcode+1].name).." ";

		if(self.lookup[_opcode+1].addrmode==self.IMP)then
			sInst = sInst.." {IMP}";
		elseif(self.lookup[_opcode+1].addrmode==self.IMM)then
			value = self.bus:Read(addr,true); addr = addr+1;
			sInst = sInst.."$"..hex(lo, 2).." {IMM}";
		elseif(self.lookup[_opcode+1].addrmode==self.ZP0)then
			lo = self.bus:Read(addr,true); addr = addr+1;
			hi = 0x00;
			sInst = sInst.."$"..hex(lo, 2).." {ZP0}";
		elseif(self.lookup[_opcode+1].addrmode==self.ZPX)then
			lo = self.bus:Read(addr,true); addr = addr+1;
			hi = 0x00;
			sInst = sInst.."$"..hex(lo, 2).." X {ZPX}";
		elseif(self.lookup[_opcode+1].addrmode==self.ZPY)then
			lo = self.bus:Read(addr,true); addr = addr+1;
			hi = 0x00;
			sInst = sInst.."$"..hex(lo, 2).." Y {ZPY}";
		elseif(self.lookup[_opcode+1].addrmode==self.IZX)then
			lo = self.bus:Read(addr,true); addr = addr+1;
			hi = 0x00;
			sInst = sInst.."($"..hex(lo, 2)..", X) {IZX}";
		elseif(self.lookup[_opcode+1].addrmode==self.IZY)then
			lo = self.bus:Read(addr,true); addr = addr+1;
			hi = 0x00;
			sInst = sInst.."($"..hex(lo, 2)..", Y) {IZY}";
		elseif(self.lookup[_opcode+1].addrmode==self.ABS)then
			lo = self.bus:Read(addr,true); addr = addr+1;
			hi = self.bus:Read(addr,true); addr = addr+1;
			sInst = sInst.."$"..hex(bitOr(bitLShift(hi, 8), lo), 4).." {ABS}";
		elseif(self.lookup[_opcode+1].addrmode==self.ABX)then
			lo = self.bus:Read(addr,true); addr = addr+1;
			hi = self.bus:Read(addr,true); addr = addr+1;
			sInst = sInst.."$"..hex(bitOr(bitLShift(hi, 8), lo), 4)..", Y {ABX}";
		elseif(self.lookup[_opcode+1].addrmode==self.ABY)then
			lo = self.bus:Read(addr,true); addr = addr+1;
			hi = self.bus:Read(addr,true); addr = addr+1;
			sInst = sInst.."$"..hex(bitOr(bitLShift(hi, 8), lo), 4)..", Y {ABY}";
		elseif(self.lookup[_opcode+1].addrmode==self.IND)then
			lo = self.bus:Read(addr,true); addr = addr+1;
			hi = self.bus:Read(addr,true); addr = addr+1;
			sInst = sInst.."$"..hex(bitOr(bitLShift(hi, 8), lo), 4).." {IND}";
		elseif(self.lookup[_opcode+1].addrmode==self.REL)then
			value = self.bus:Read(addr,true); addr = addr+1;
			sInst = sInst.."$"..hex(value, 2).." [$"..hex(addr+value, 4).."] {REL}";
		end

		mapLines[line_addr+1] = sInst;
	end
	return mapLines;
end