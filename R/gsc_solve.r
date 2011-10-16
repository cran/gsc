gsc_solve = function (m, x, y, order=4, reweight=TRUE, p=0.98)
{	n = length (x)
	if (n < 3)
		stop ("solve.gsc requires at least 3 points")
	if (reweight)
		p = 1 + (p - 1) / n^order
	q = 1 - p
	Dmat = .arg_Dmat (order, n, q, p)
	dvec = .arg_dvec (q, y)
	k = .args_constraints (m, x)
	solve.QP (Dmat=Dmat, dvec=dvec, Amat=k$Amat, meq=k$meq)$solution
}

.arg_Dmat = function (order, n, q, p)
{	Dmat = matrix (0, n, n)
	diag (Dmat) = 2 * q
	k = 0:(n - order - 1)
	z = 2 * .diffcoef.local (order)
	for (i in 1:(order + 1) )
		for (j in 1:(order + 1) )
			Dmat [i + k, j + k] = Dmat [i + k, j + k] + z [i, j]
	Dmat
}

.arg_dvec = function (q, y)
	cbind (2 * q * y)

.diffcoef.local = function (order)
{	z = if (order == 2) c (1, -2, 1)
	else if (order == 3) c (1, -3, 3, -1)
	else if (order == 4) c (1, -4, 6, -4, 1)
	else if (order == 5) c (1, -5, 10, -10, 5, -1)
	else if (order == 6) c (1, -6, 15, -20, 15, -6, 1)
	else stop ("gsc_solve requires order 2 to 6")
	cbind (z) %*% rbind (z)
}

