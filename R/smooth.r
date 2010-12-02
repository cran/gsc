#todo (in ofp): update SMOOTH arguments
#todo (in ofp): allow zero degree
smoothc = function (g, x, y, ns=50, degree=2, smoothness=0.67,
	smooth.in=TRUE, smooth.out=FALSE)
{	smoothness = smoothness * diff (range (x) )
	if (smooth.in)
	{	f.in = SMOOTH (x, y, n=ns, degree=degree, smoothness=smoothness)
		x = f.in$x
		y = f.in$y
	}
	y = fit (g, x, y)
	if (smooth.out)
	{	f.out = SMOOTH (x, y, n=ns, degree=degree, smoothness=smoothness)
		x = f.out$x
		y = f.out$y
	}
	extend (SERIES (x, y), "smoothc")	
}


