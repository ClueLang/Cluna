local function fizzbuzz(number)
	local string = "";
	if number%3==0 then
		string = string .. "fizz";
	end
	if number%5==0 then
		string = string .. "buzz";
	end
	if string~="" then
		print(string..": "..number);
	end
end
for i = 1, 100, 1 do
	fizzbuzz(i);
end