#Outdated needs to be updated
`43;;`

`if true then 2 else 3;;`

'let x = 2;;'

'let y = x + 2;;'

'let addTogether (arg1 : int, arg2 : int) =
	arg1 + arg2;;
'

'let addMultipleTogether (arg1 : int, arg2 : int, arg3 : int, arg4 : int) =
	addTogether ( addTogether (arg1, arg2) addTogether (arg3, arg4) );;
'

'addTogether (x, y);;'

'addMultipleTogether (x, x, y, y);;'