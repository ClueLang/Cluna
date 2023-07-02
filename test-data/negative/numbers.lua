-- Negative tests for invalid Lua number formats
-- Invalid exponent notation
local num = 1.0e

-- Invalid hexadecimal notation
local num = 0x

-- Invalid hexadecimal exponent notation
local num = 0x1p

-- Invalid negative exponent notation
local num = 1.0e-

-- Invalid positive exponent notation
local num = 1.0e+

-- Invalid negative hexadecimal exponent notation
local num = 0x1p-

-- Invalid positive hexadecimal exponent notation
local num = 0x1p+

-- Invalid hexadecimal exponent notation
local num = 0x1p1.0

-- Invalid hexadecimal exponent notation
local num = 0x1p-1.0

-- Invalid hexadecimal exponent notation
local num = 0x1p+1.0

-- Invalid hexadecimal exponent notation
local num = 0x1p1e1

-- Invalid hexadecimal exponent notation
local num = 0x1p-1e1

-- Invalid hexadecimal exponent notation
local num = 0x1p+1e1
