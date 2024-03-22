var a = rand(5,15);
var b = rand(3,8);

if (a < 0) a = -a;
if (b < 0) b = -b;
while (b > 0) {  // try changing this to b >= 0
  var tmp = b;
  b = a % b;
  a = tmp;
 }
assert (b==0); // should be true
//assert (a>6); // try uncommenting this line
var tmp = 0;
print (a);

