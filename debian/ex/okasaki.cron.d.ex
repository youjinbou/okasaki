#
# Regular cron jobs for the okasaki package
#
0 4	* * *	root	[ -x /usr/bin/okasaki_maintenance ] && /usr/bin/okasaki_maintenance
