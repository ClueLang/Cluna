local function ReverseBits(n)
	local rev = 0;
	while (n>0) do
		rev = rev<<1;
		if (n&1)==1 then
			rev = rev^^1;
		end
		n = n>>1;
	end
	return rev;
end
print(ReverseBits(11));