var a = -26134272;
var b = 26396928;

if (a < 0) a = -a;
if (b < 0) b = -b;
while (b > 0) {  // try changing this to b >= 0
  var tmp = b;
  b = a % b;
  a = tmp;
 }
print (a); // this should print 131328

