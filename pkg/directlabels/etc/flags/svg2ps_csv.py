import svg2ps,csv
def convert_csv(svg_csv_file, ps_csv_file):
    """convert all files listed in the specified csv file."""
    f = open(svg_csv_file)
    out = open(ps_csv_file,"w")
    reader = csv.reader(f,lineterminator="\n",delimiter=",")
    writer = csv.writer(out,lineterminator="\n",delimiter=",")
    names_tup = reader.next()
    writer.writerow( ("svg","ps") )
    i = names_tup.index("filename")
    for tup in reader:
        svg_filename = tup[i]
        ps_filename = svg2ps.convert(svg_filename)
        writer.writerow( (svg_filename, ps_filename) )

if __name__ == "__main__":
    import sys
    if len(sys.argv)!=3:
        print """Usage: %s svg_files.csv ps_files.csv
svg_files.csv must contain a column named 'filename' and
ps_files.csv will be written."""%sys.argv[0]
        sys.exit(1)
    convert_csv(sys.argv[1], sys.argv[2])
