gsc = function (k=numeric (), s0=NA, s1=NA, s2=NA, s3=NA)
{	s0 = .resolve (k, s0)
	s1 = .resolve (k, s1)
	s2 = .resolve (k, s2)
	s3 = .resolve (k, s3)
	nk = length (k)
	extend (LIST (), "gsc", k, s0, s1, s2, s3, nk, ns=nk + 1L)
}

.constraint.matrix = function (g, x)
{	neq = 0
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
	LIST (m, neq)
}

#todo: validate symbols {0, +, -, ?}
#todo: allow character vectors
.resolve = function (k, s)
{	ns = length (k) + 1L
	if (! is.na (s) )
	{	s = strsplit (s, "") [[1]]
		s [s == "?"] = NA
	}
	nc = length (s)
	if (nc == ns) s
	else if (nc > ns) stop ("too many segment constraints specified")
	else if (ns %% nc != 0) stop ("segment constraints couldn't be recycled")
	else rep (s, ns / nc)
}

.indices = function (g, x)
{	nj = jmin = jmax = numeric (g$ns) 
	if (g$ns == 1)
	{	nj = length (x)
		jmin = 1
		jmax = length (x)
	}
	else
{
#modify k values (so all k in x)
for (i in 1:g$nk)
{	m = which.min (abs (g$k [i] - x) )
	g$k [i] = x [m]
}
#in lines '#*', using '<=' (instead of '<')
		for(i in 1:g$ns)
		{	j = if (i == 1)
				(x <= g$k [1]) #*
			else if (i == g$ns)
				(x >= g$k [g$nk])
			else
				(x >= g$k [i -1] & x <= g$k [i]) #*
			j = which (j)
#overlap
#if (j [1] > 1) j = c (j [1] - 1, j)
#if (j [length (j)] < length (x) ) j = c (j, j [length (j)] + 1)
			nj [i] = length (j)
			if (nj [i] > 0)
			{	jmin [i] = min (j)
				jmax [i] = max (j)
			}
		}
}
	LIST (nj, jmin, jmax)
}


