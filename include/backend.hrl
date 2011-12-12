-record(account, {no,
        balance=0,
        pin,
        name,
        transactions=[]}).

-define(VALID_BANKS,
    [{"royal bank", 1234}, {"commercial bank", 2384},
     {"friendly bank", 1231}, {"fake bank", 123012}]).
