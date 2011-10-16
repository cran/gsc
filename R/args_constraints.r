#todo: horrible, rewrite...
.args_constraints = function (g, x)
{	#source from version 0.2.1
	neq = 0
	nx = length (x)
	meq = mineq = matrix (0, 0, nx)
	z = .indices (g, x)
	for (i in 1:g$ns)
	{	if (z$nj [i] > 0)
		{	for (j in z$jmin [i]:z$jmax [i])
			{	r = rep (0, nx)
				if (is.na (g$s0 [i]) ) {}
				else if (g$s0 [i] == "0")
				{	neq = neq + 1
					r [j] = 1
					meq = rbind (meq, r)
				}
				else
				{	if (g$s0 [i] == "+") r [j] = 1
					else if (g$s0 [i] == "-") r [j] = -1
					mineq = rbind (mineq, r)
				}
			}
		}
		if (z$nj [i] > 1)
		{	for (j in z$jmin [i]:(z$jmax [i] - 1) )
			{	r = rep (0, nx)
				if (is.na (g$s1 [i]) ) {}
				else if (g$s1 [i] == "0")
				{	neq = neq + 1
					r [j] = 1
					r [j + 1] = 1
					meq = rbind (meq, r)
				}
				else
				{	if (g$s1 [i] == "+")
					{	r [j] = -1
						r [j + 1] = 1
					}
					else if (g$s1 [i] == "-")
					{	r [j] = 1
						r [j + 1] = -1
					}
					mineq = rbind (mineq, r)
				}
			}
		}
		if (z$nj [i] > 2)
		{	for (j in z$jmin [i]:(z$jmax [i] - 2) )
			{	r = rep (0, nx)
				if (is.na (g$s2 [i]) ) {}
				else if (g$s2 [i] == "0")
				{	neq = neq + 1
					r [j] = 1
					r [j + 1] = -2
					r [j + 2] = 1
					meq = rbind (meq, r)
				}
				else
				{	if (g$s2 [i] == "+")
					{	r [j] = 1
						r [j + 1] = -2
						r [j + 2] = 1
					}
					else if (g$s2 [i] == "-")
					{	r [j] = -1
						r [j + 1] = 2
						r [j + 2] = -1
					}
					mineq = rbind (mineq, r)
				}
			}
		}
		if (z$nj [i] > 3)
		{	for (j in z$jmin [i]:(z$jmax [i] - 3) )
			{	r = rep (0, nx)
				if (is.na (g$s3 [i]) ) {}
				else if (g$s3 [i] == "0")
				{	neq = neq + 1
					r [j] = -1
					r [j + 1] = 3
					r [j + 2] = -3
					r [j + 3] = 1
					meq = rbind (meq, r)
				}
				else
				{	if (g$s3 [i] == "+")
					{	r [j] = -1
						r [j + 1] = 3
						r [j + 2] = -3
						r [j + 3] = 1
					}
					else if (g$s3 [i] == "-")
					{	r [j] = 1
						r [j + 1] = -3
						r [j + 2] = 3
						r [j + 3] = -1
					}
					mineq = rbind (mineq, r)
				}
			}
		}
	}
	if (neq > 0) row.names (meq) = paste ("eq", 1:neq, sep="")
	if (nrow (mineq) > 0) row.names (mineq) = 1:nrow (mineq)
	m = rbind (meq, mineq)

	#source removed version 0.3.0
	#LIST (m, neq)

	#source added version 0.3.0
	list (Amat=t (m), meq=neq)
}

.indices = function (m, x)
{	n = a = b = rep (NA, m$ns)
	if (m$ns == 1)
	{	n = b = length (x)
		a = 1
	}
	else
		for (z in 1:m$ns)
		{	i = if (z == 1) (x <= m$knots [1])
			else if (z == m$ns) (x >= m$knots [m$nk])
			else (x >= m$knots [z - 1] & x <= m$knots [z])
			i = which (i)
			n [z] = length (i)
			if (n [z] > 0)
			{	a [z] = min (i)
				b [z] = max (i)
			}
		}

	#for version 0.2.1
	list (nj=n, jmin=a, jmax=b)
}


