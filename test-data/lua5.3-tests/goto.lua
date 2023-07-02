-- $Id: goto.lua,v 1.13 2016/11/07 13:11:28 roberto Exp $
-- See Copyright Notice in file all.lua

collectgarbage()

local function errmsg(code, m)
  local st, msg = load(code)
  assert(not st and string.find(msg, m))
end

-- cannot see label inside block
errmsg([[ goto l1; do ::l1:: end ]], "label 'l1'")
errmsg([[ do ::l1:: end goto l1; ]], "label 'l1'")

-- repeated label
errmsg([[ ::l1:: ::l1:: ]], "label 'l1'")


-- undefined label
errmsg([[ goto l1; local aa ::l1:: ::l2:: print(3) ]], "local 'aa'")

-- jumping over variable definition
errmsg([[
do local bb, cc;  end
local aa
 print(3)
]], "local 'aa'")

-- jumping into a block
errmsg([[ do ::l1:: end goto l1 ]], "label 'l1'")
errmsg([[ goto l1 do ::l1:: end ]], "label 'l1'")

-- cannot continue a repeat-until with variables
errmsg([[
  repeat
    if x then  end
    local xuxu = 10

  until xuxu < x
]], "local 'xuxu'")

-- simple gotos
local x
do
  local y = 12


  x = x + 1;

  x = y;
end


assert(x == 13)


-- long labels
do
  local prog = [[
  do
    local a = 1
     a = a + 1
    a = a + 10
     a = a + 2
    a = a + 20
    return a
  end
  ]]
  local label = string.rep("0123456789", 40)
  prog = string.format(prog, label, label, label, label)
  assert(assert(load(prog))() == 31)
end

--  correct label when nested
do end  -- does not loop jumping to previous label 'l3'

-- ok to jump over local dec. to end of block
do
  local a = 23
  x = a

  ;
end

while true do
  -- ok to jump over local dec. to end of block
  -- multiple uses of same label
  local x = 45

  ;;;
end

assert(x == 13)

if print then
  -- ok to jump over local dec. to end of block
  error("should not be here")
  -- ok to jump over local dec. to end of block
  local x

  ;
  ;;
else
end

-- to repeat a label in a different function is OK
local function foo()
  local a = {}


  a[#a + 1] = 1;

  a[#a + 1] = 2;


  a[#a + 1] = 3;

  a[#a + 1] = 4;

  a[#a + 1] = 5;

  assert(a[1] == 3 and a[2] == 1 and a[3] == 2 and
    a[4] == 5 and a[5] == 4)
  if not a[6] then a[6] = true; end -- do it twice
end


foo()


do -- bug in 5.2 -> 5.3.2
  local x

  local y -- cannot join this SETNIL with previous one
  assert(y == nil)
  y = true
  if x == nil then
    x = 1
  else
    x = x + 1
  end
  assert(x == 2 and y == true)
end

--------------------------------------------------------------------------------
-- testing closing of upvalues

local debug = require 'debug'

local function foo()
  local t = {}
  do
    local i = 1
    local a, b, c, d
    t[1] = function() return a, b, c, d end

    local b
    do
      local c
      t[#t + 1] = function() return a, b, c, d end -- t[2], t[4], t[6]
      if i > 2 then end
      do
        local d
        t[#t + 1] = function() return a, b, c, d end -- t[3], t[5]
        i = i + 1
        local a
      end
    end
  end

  return t
end

local a = foo()
assert(#a == 6)

-- all functions share same 'a'
for i = 2, 6 do
  assert(debug.upvalueid(a[1], 1) == debug.upvalueid(a[i], 1))
end

-- 'b' and 'c' are shared among some of them
for i = 2, 6 do
  -- only a[1] uses external 'b'/'b'
  assert(debug.upvalueid(a[1], 2) ~= debug.upvalueid(a[i], 2))
  assert(debug.upvalueid(a[1], 3) ~= debug.upvalueid(a[i], 3))
end

for i = 3, 5, 2 do
  -- inner functions share 'b'/'c' with previous ones
  assert(debug.upvalueid(a[i], 2) == debug.upvalueid(a[i - 1], 2))
  assert(debug.upvalueid(a[i], 3) == debug.upvalueid(a[i - 1], 3))
  -- but not with next ones
  assert(debug.upvalueid(a[i], 2) ~= debug.upvalueid(a[i + 1], 2))
  assert(debug.upvalueid(a[i], 3) ~= debug.upvalueid(a[i + 1], 3))
end

-- only external 'd' is shared
for i = 2, 6, 2 do
  assert(debug.upvalueid(a[1], 4) == debug.upvalueid(a[i], 4))
end

-- internal 'd's are all different
for i = 3, 5, 2 do
  for j = 1, 6 do
    assert((debug.upvalueid(a[i], 4) == debug.upvalueid(a[j], 4))
      == (i == j))
  end
end

--------------------------------------------------------------------------------
-- testing if x goto optimizations

local function testG(a)
  if a == 1 then
    error("should never be here!")
  elseif a == 2 then
  elseif a == 3 then
  elseif a == 4 then
    -- go to inside the block
    error("should never be here!")

    a = a + 1 -- must go to 'if' end
  else
    a = a * 2;
    error("should never be here!")

    error("should never be here!")
  end
  do return a end

  do return "2" end

  do return "3" end

  return "1"
end

assert(testG(1) == "1")
assert(testG(2) == "2")
assert(testG(3) == "3")
assert(testG(4) == 5)
assert(testG(5) == 10)
--------------------------------------------------------------------------------


print 'OK'
