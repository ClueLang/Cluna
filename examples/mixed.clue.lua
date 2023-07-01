local _internal0;
if condition then
	_internal0 = "true";
else
	_internal0 = "false";
end
local bool_result = _internal0;
local result = 0;
local output = "[Output] ";
local _internal1 = io.read("*n");
if (_internal1 == 1) then
	_internal1 = 1;
else
	if (_internal1 == 2) or (_internal1 == 3) then
		_internal1 = 10;
	else
		_internal1 = nil;
	end
end
result = _internal1;
output = output .. bool_result.." "..result;
print(output);