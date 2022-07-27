local _t0;
if condition then
	_t0 = "true";
else
	_t0 = "false";
end
local bool_result = _t0;
local result = 0;
local output = "[Output] ";
local _match = user_input;
if (_match == 1) then
	result = 1;
elseif (_match == 2) or (_match == 3) then
	result = 10;
end
output = output .. bool_result.." "..result;
print(output);