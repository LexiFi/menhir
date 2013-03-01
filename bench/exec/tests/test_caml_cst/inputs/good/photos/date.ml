let date year month day =
  let date, _ = Unix.mktime {
		  Unix.tm_year = year - 1900;
		  Unix.tm_mon = month - 1;
		  Unix.tm_mday = day;
		  Unix.tm_hour = 0;
		  Unix.tm_min = 0;
		  Unix.tm_sec = 0;
		  Unix.tm_wday = 0;
		  Unix.tm_yday = 0;
		  Unix.tm_isdst = false
		} in
  date, date +. 86400.0

