netmailbot ^
-to ignored ^
-from "<<from>>" ^
-subject "PV Car & Truck Rentals" ^
-server smtp.bizmail.yahoo.com ^
-authlogin pvrez@eonsum.com -authpassword emailacct ^
-logfile "log.txt" ^
-dsn "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=\netmail\;Extended properties=Text;" ^
-dbquery "SELECT * FROM radata.txt" ^
-dbemailcolumn "to" ^
-dbreplacementids "<<to>>=to,<<from>>=from,<<location>>=location,<<customer>>=customer,<<addcust>>=addcust,<<vehicle>>=vehicle,<<return>>=return,<<rateinfo>>=rateinfo,<<racharge>>=racharge,<<payment>>=payment" ^
-bodyfile "ramsg.htm" ^
-personalize ^
-debug


