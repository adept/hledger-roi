# hledger-roi
Hledger Return-on-investment calucator. Does time-weighted (TWR) and money-weighted (IRR) computations

Both IRR and TWR give you useful approximations that you can use to make your investment decision. 

In reality returnon your investments will change day to day. This is really hard to think about, so for simplicity we could instead assume that your investment growns at a constant rate R. We need to choose R in such a way that all your cash contributions to your investment, grown at this rate for the respective periods of time they were invested, give you the current value of this portfolio. This rate is called money-weighted or dollar-weighter rate of return or **internal rate of return -- IRR for short**.

**Time weighted returns (TWR)** on the other hand ignores the effect of cash inflows/outflows, but recognizes that different periods of time have different rates of return and attempts to tell you what is the performance of your investment portfolio over time that you held it. This is also commonly called "return on the first dollar invested".

Good description of differences between IRR and TWR can be found at http://www.merriman.com/investing-101/performance-time-weighted-return-vs-internal-rate-of-return/, and in Investopedia (check references below).

# Usage
This tool assumes that you have account(s) that hold nothing but your investments and whenever you record current valuation of these inestments you offset unrealized profit and loss into account(s) that, again, hold nothing but unrealized profit and loss.

Any transactions affecting balance of investment account(s) and not originating from unrealized profit and loss account(s) are assumed to be your investments or withdrawals.

At a minimum, you need to supply hledger query (this could be just an account name) to select your investments via `-i` switch, and another query to identify your profit and loss transactions via `-P` switch.

Tool will compute and display internalized rate of return (IRR) and time-weighted rate of return (TWR) for your investments for the time period requested. Both rates of return are annualized before display, regardless of the length of reporting interval.

# Command line switches
```
compute return-on-investment for your portfolio using IRR and TWR

hledger-roi [OPTIONS]

Common flags:
  -c --cashflow           also show all revant transactions
  -d --debug              print debugging info
  -f --file=FILE          input ledger file (pass '-' for stdin)
  -i --investments=QUERY  investments query
  -P --pnl=QUERY          profit-and-loss query
  -b --begin=DATE         calculate interest from this date
  -e --end=DATE           calculate interest until this date
  -D --daily              calculate interest for each day
  -W --weekly             calculate interest for each week
  -M --monthly            calculate interest for each month
  -Y --yearly             calculate interest for each year
  -p --period=PERIODEXP   set start date, end date, and/or report interval
                          all at once (overrides the flags above)
  -? --help               Display help message
  -V --version            Print version information
```

# Examples
These examples use sample.journal which you can find in the repo.

Computing IRR/TWR for the whole period:
```
> ./hledger-roi.hs -f sample.journal -i 'investment' -P 'unrealized pnl' 
+---++------------+------------++---------------+----------+-------------+------++--------+--------+
|   ||      Begin |        End || Value (begin) | Cashflow | Value (end) |  PnL ||    IRR |    TWR |
+===++============+============++===============+==========+=============+======++========+========+
| 1 || 2013/04/17 | 2018/09/11 ||           576 |    16660 |       22720 | 5484 || 11.33% | 10.12% |
+---++------------+------------++---------------+----------+-------------+------++--------+--------+
```

Display cash flows that went into each computation:
```
> ./hledger-roi.hs -f sample.journal -i 'investment' -P 'unrealized pnl' -c

IRR cash flow for 2013/04/17 - 2018/09/11
+------------++--------+
|            || Amount |
+============++========+
| 2013/04/17 ||   -576 |
| 2013/07/05 ||  -1150 |
| 2014/05/19 ||  -1150 |
| 2014/05/23 ||  -1180 |
| 2014/07/21 ||   -340 |
| 2014/07/24 ||   -310 |
| 2015/04/14 ||  -1520 |
| 2015/04/17 ||  -1520 |
| 2017/04/05 ||  -3040 |
| 2017/04/17 ||  -2450 |
| 2018/04/05 ||  -4000 |
| 2018/09/11 ||  22720 |
+------------++--------+


TWR cash flow for 2013/04/17 - 2018/09/11
+------------++--------------++---------+------------+-------++------------------+
|            || Unit balance ||    Cash | Unit price | Units || New Unit Balance |
+============++==============++=========+============+=======++==================+
| 2013/04/17 ||         0.00 ||  576.00 |     100.00 |  5.76 ||             5.76 |
| 2013/07/05 ||         5.76 || 1150.00 |     104.34 | 11.02 ||            16.78 |
| 2014/05/19 ||        16.78 || 1150.00 |     111.19 | 10.34 ||            27.12 |
| 2014/05/23 ||        27.12 || 1180.00 |     111.27 | 10.61 ||            37.73 |
| 2014/07/21 ||        37.73 ||  340.00 |     112.27 |  3.03 ||            40.76 |
| 2014/07/24 ||        40.76 ||  310.00 |     112.47 |  2.76 ||            43.51 |
| 2015/04/14 ||        43.51 || 1520.00 |     108.66 | 13.99 ||            57.50 |
| 2015/04/17 ||        57.50 || 1520.00 |     108.72 | 13.98 ||            71.48 |
| 2017/04/05 ||        71.48 || 3040.00 |     143.29 | 21.22 ||            92.70 |
| 2017/04/17 ||        92.70 || 2450.00 |     143.38 | 17.09 ||           109.79 |
| 2018/04/05 ||       109.79 || 4000.00 |     159.08 | 25.14 ||           134.93 |
+------------++--------------++---------+------------+-------++------------------+
Final unit price: 22720.00/134.93=168.38 U.
Total TWR: 68.38%.
Period: 5.41 years.
Annualized TWR: 10.12%

+---++------------+------------++---------------+----------+-------------+------++--------+--------+
|   ||      Begin |        End || Value (begin) | Cashflow | Value (end) |  PnL ||    IRR |    TWR |
+===++============+============++===============+==========+=============+======++========+========+
| 1 || 2013/04/17 | 2018/09/11 ||           576 |    16660 |       22720 | 5484 || 11.33% | 10.12% |
+---++------------+------------++---------------+----------+-------------+------++--------+--------+
```

Computing IRR/TWR per year:
```
> ./hledger-roi.hs -f sample.journal -i 'investment' -P 'unrealized pnl' -Y                                                                                       ~/devel/haskell/darcs-get/hledger-roi
+---++------------+------------++---------------+----------+-------------+------++--------+--------+
|   ||      Begin |        End || Value (begin) | Cashflow | Value (end) |  PnL ||    IRR |    TWR |
+===++============+============++===============+==========+=============+======++========+========+
| 1 || 2013/01/01 | 2014/01/01 ||             0 |     1726 |        1840 |  114 || 11.96% |  9.64% |
| 2 || 2014/01/01 | 2015/01/01 ||          1840 |     2980 |        5142 |  322 ||  9.11% |  7.78% |
| 3 || 2015/01/01 | 2016/01/01 ||          5142 |     3040 |        7911 | -271 || -3.70% | -6.35% |
| 4 || 2016/01/01 | 2017/01/01 ||          7911 |        0 |       10243 | 2332 || 29.39% | 29.39% |
| 5 || 2017/01/01 | 2018/01/01 ||         10243 |     5490 |       16974 | 1241 ||  8.74% |  7.90% |
| 6 || 2018/01/01 | 2019/01/01 ||         16974 |     4000 |       22720 | 1746 ||  8.77% |  8.91% |
+---++------------+------------++---------------+----------+-------------+------++--------+--------+
```

# Acknowledgements
This tool is originally based on `haskell-irr` by Joachim Breitner <mail@joachim-breitner.de>
Notable changes from `haskell-irr` include ability to operate on hledger queries instead of account names and handling of multi-commodity accounts (provided that all commodities are convertable to a single cost basis).

# References
* TWR computations used here are described in a lot of detail on http://monevator.com/how-to-unitize-your-portfolio/
* Good set of articles on IRR, TWR, value of money, disconting, EAR etc on Investopedia: https://www.investopedia.com/exam-guide/cfa-level-1/quantitative-methods/default.asp
