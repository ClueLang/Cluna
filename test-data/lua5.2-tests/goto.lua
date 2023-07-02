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
do

end -- does not loop jumping to previous label 'l3'

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
  if not a[6] then
    a[6] = true;
  end -- do it twice
end


foo()



--------------------------------------------------------------------------------
-- testing closing of upvalues

local function foo()
  local a = {}
  do
    local i = 1
    local k = 0
    a[0] = function(y) k = y end

    do
      local x
      if i > 2 then end
      a[i] = function(y) if y then x = y else return x + k end end
      i = i + 1
    end
  end

  return a
end

local a = foo()
a[1](10); a[2](20)
assert(a[1]() == 10 and a[2]() == 20 and a[3] == nil)
a[0](13)
assert(a[1]() == 23 and a[2]() == 33)

--------------------------------------------------------------------------------
-- testing if x

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
