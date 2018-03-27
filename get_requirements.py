import os
inn=False
out="Requirements = ("
a=os.popen("ls -s *.out").read()
for x in a.split('\n'):
    if x=='' or x[0]=='0':
        continue
    mac=x.split()[1].split('.')[0]+'.recod'
    print(mac)
    if not inn:
        out+='(Machine == "'+mac+'")'
        inn=True
    else:
        out+=' || (Machine == "'+mac+'")'
out+=' ) '
print(out)
