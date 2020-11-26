Bus = {};
Bus.__index = Bus;

function Bus:Create(processor)
	local t = setmetatable({}, Bus);

	t.Processor = processor;

	-- 64Kb ram
	t.ram = {};
	for i=0,0xFFFF do
		t.ram[i] = 0x00;
	end

	t.Processor:ConnectBus(t);

	return t;
end

function Bus:Write(addr, data)
	if(addr>=0x0000 and addr<=0xFFFF)then
		self.ram[addr] = data;
	end
end

function Bus:Read(addr, readOnly)
	if(addr>=0x0000 and addr<=0xFFFF)then
		return self.ram[addr];
	end

	return 0x0;
end