if condition then
	if condition2 then
		print("condition2");
	else
		print("condition");
	end
else
	print("no condition");
end
local _internal0 = condition;
if ((_internal0 == true)) and condition2 then
	print("condition2");
else
	if (_internal0 == true) then
		print("condition");
	else
		if (_internal0 == false) then
			print("no condition");
		end
	end
end