local vector = setmetatable({
	x = 0, 
	y = 0
}, {
	__unm = function(t)
		return {
			-t.x, 
			-t.y
		};
	end, 
	__tostring = function(t)
		return "vector("..t.x..", "..t.y..")";
	end, 
	__add = function(a, b)
		return {
			a.x+b.x, 
			a.y+b.y
		};
	end, 
	__sub = function(a, b)
		return {
			a.x-b.x, 
			a.y-b.y
		};
	end, 
	__mul = function(a, b)
		return {
			a.x*b.x, 
			a.y*b.y
		};
	end, 
	__div = function(a, b)
		return {
			a.x/b.x, 
			a.y/b.y
		};
	end, 
	__eq = function(a, b)
		return a.x==b.x and a.y==b.y;
	end
});