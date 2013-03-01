import rsvg
import cairo
def convert(svg_file):
    """Convert the named svg file to a ps file using cairo.

    Returns the name of the new ps file."""
    svg=rsvg.Handle(svg_file)
    ps_file=svg_file.replace(".svg",".ps")
    d=svg.get_dimension_data()
    ps=cairo.PSSurface(ps_file,d[0],d[1])
    context=cairo.Context(ps)
    svg.render_cairo(context)
    ps.finish()
    return ps_file
if __name__=="__main__":
    import sys
    for f in sys.argv[1:]:
        print f
        convert(f)
