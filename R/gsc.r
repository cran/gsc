#todo: change notation
#note: "s" used ambiguously, segment (e.g. ns) and shape (e.g. s0)
gsc = function (s0=NA, s1=NA, s2=NA, s3=NA, knots=numeric () )
{	m = list ()
	class (m) = "gsc"
	m$nk = length (knots)
	m$ns = m$nk + 1L
	m$knots = knots
	m$s0 = .resolve (m$ns, s0)
	m$s1 = .resolve (m$ns, s1)
	m$s2 = .resolve (m$ns, s2)
	m$s3 = .resolve (m$ns, s3)
	m
}

print.gsc = function (x, ...)
	.print.gsc (m=x, ...)

.print.gsc = function (m, ...)
{	cat ("gsc object\n----------\n")
	cat ("s0:", .formatc (m$s0), "\n")
	cat ("s1:", .formatc (m$s1), "\n")
	cat ("s2:", .formatc (m$s2), "\n")
	cat ("s3:", .formatc (m$s3), "\n")
	if (m$nk == 0) cat ("knots: none\n")
	else cat ("knots:", paste (m$knots, collapse=", "), "\n")
}

.resolve = function (ns, s)
{	if (!is.na (s) )
	{	s = strsplit (s, "") [[1]]
		s [s == "?"] = NA
	}
	nc = length (s)
	if (nc == ns) s
	else if (nc > ns) stop ("too many segment constraints specified")
	else if (ns %% nc != 0) stop ("segment constraints couldn't be recycled")
	else rep (s, ns %/% nc)
}

.formatc = function (s)
{	k = is.na (s)
	if (all (k) ) "none"
	else
	{	s [k] = "?"
		paste (s, collapse="")
	}
}


