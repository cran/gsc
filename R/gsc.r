gsc = function (k=numeric (), d0=NA, d1=NA, d2=NA)
{	d0 = .resolve (k, d0)
	d1 = .resolve (k, d1)
	d2 = .resolve (k, d2)
	nk = length (k)
	extend (LIST (), "gsc", k, d0, d1, d2, nk, ns=nk + 1)
}

.resolve = function (k, d)
{	if (is.character (d) )
	{	d = strsplit (d, "") [[1]]
		x = numeric ()
		for (i in 1:length (d) )
		{	if (d [i] == "0") x [i] = 0
			else if (d [i] == "+") x [i] = 1
			else if (d [i] == "-") x [i] = -1
			else if (d [i] == "?") x [i] = NA
			else stop ("invalid constraint symbol")
		}
		d = x
	}
	ns = length (k) + 1
	nd = length (d)
	if (ns < nd) stop ("too many segment constraints specified")
	else if (ns %% nd != 0) stop ("segment constraints couldn't be recycled")
	else rep (as.integer (d), ns / nd)
}

