stores_q1 = [890771, 828168, 'store_31', '829826']
stores_q2 = [824242, 825587, 826774]
stores_q3 = [830420, 832156, 832267]
stores_q4 = [834442, 'store_57', 834782, 839535]
online_q1 = [331941, 334578, 339944]
online_q2 = [341097, 'May_25', 342657, 345157]
online_q3 = [351017, 353720, 355999]
online_q4 = ['358235', 361290, 'November_12', 364395]

def annual_sales(stores_q1, stores_q2, stores_q3, stores_q4, online_q1, online_q2, online_q3, online_q4):
    l = [stores_q1, stores_q2, stores_q3, stores_q4, online_q1, online_q2, online_q3, online_q4]
    q = 0
    for i in l:
        for n in i:
            try:
                x = int(n)
                q += x
            except:
                print('fuck')
    return(q)
print(annual_sales(stores_q1, stores_q2, stores_q3, stores_q4, online_q1, online_q2, online_q3, online_q4))
