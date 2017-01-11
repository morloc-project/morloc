run_mid=$0
null() {
$1 $@ > /dev/null
}
datcache_chk() {
test -f $1.dat
}
datcache_get() {
cat $1.dat
}
datcache_put() {
cat > $1.dat
}
id() {
cat $1
}
run() {
$@
}
nothing() {
echo -n
}
f() {
echo $(( 5 * 7 ))
}
g() {
echo $(( 4 * 5 ))
}
m0(){
if datcache_chk $mid
then
datcache_get $mid
else
run g OPEN_m1  |
tee >(null scrollit) |tee >(null printit) |
id | datcache_put $mid
fi
}
m1(){
if isgood && issupergood
then
doit f  -x 1 -y 2 |
outputzipit 
else
diehorribly
fi
}
