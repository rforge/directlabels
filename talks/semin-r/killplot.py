import sys
f=sys.argv[1]
txt=open(f).read()
f=open(f,'w')
for t in txt.split("Sinput"):
    if "plot" in t:
        t=t.replace("plot(","")
        i=t.rindex(")")
        t=t[:i]+t[i+1:]
    f.write(t+"Sinput")
