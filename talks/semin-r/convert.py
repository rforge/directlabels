#!/usr/bin/python3
import re #Regular Expressions
import pdb #Python DeBugger
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
        d['settings']='show.settings' in d['code']
        d['print']='head' in d['code'] or 'print' in d['code']
        d['addplot']=not d['print'] and not d['settings']
        d['fig']=d['settings'] or d['addplot']
        d['args']=re.sub('[ ."=]',"-",d['title'])
        d['args']+=",fig=T,width=10" if d['fig'] else ""
        if d['addplot']:
            lines=d['code'].split('\n')
            d['code']='\n'.join(lines[:-1]+['plot('+lines[-1]+')'])
    return ms

latex=open('HOCKING-latticedl-semin-r-in.Rnw').read()
for block in "LATTICEDL","LATTICE":
    chunks=[TEMPLATE%d for d in parseR(block+".R")]
    latex=latex.replace(block,'\n'.join(chunks))
print(latex)
