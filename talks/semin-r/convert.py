#!/usr/bin/python
import re,pdb
args={True:"fig=T,width=10",False:""}
TEMPLATE=r"""
\frame[containsverbatim]{\frametitle{%(title)s}
<<%(args)s>>=
%(code)s
@
}
"""
def parseR(f):
    text=open(f).read()
    ms=[m.groupdict() for m in 
        re.finditer(r'##(?P<title>.*?)\n(?P<code>.*?)\n\n',text,re.DOTALL)]
    for d in ms:
        d['isplot']=not ('head' in d['code'] or 'print' in d['code'])
        d['args']=args[d['isplot']]
        if d['isplot']:
            lines=d['code'].split('\n')
            d['code']='\n'.join(lines[:-1]+['plot('+lines[-1]+')'])
    return ms

latex=open('HOCKING-latticedl-semin-r-in.Rnw').read()
for block in "LATTICEDL","LATTICE":
    chunks=[TEMPLATE%d for d in parseR(block+".R")]
    latex=latex.replace(block,'\n'.join(chunks))
print latex
