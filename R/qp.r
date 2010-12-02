fit.gsc = function (g, x, y, ...)
{	n = length (x)
	if (missing (y) )
	{	if (g$ns == 1)
			stop ("both x and y must be specified, except with global constraints")
		y = x
		x = 1:n
	}
	Dmat = matrix (0, n, n)
	diag (Dmat) = 2
	dvec = 2 * y
	m = .constraint.matrix (g, x)
	solve.QP (Dmat=Dmat, dvec=dvec, Amat=t (m$m), meq=m$neq)$solution
}


