$PROB ABS ONE COMP PK MODEL

$PARAM @annotated
KA 		: 0.5 : Absorption Rate Constant (1/hr)
CL 		: 5   : Clearance (L/hr)
V1 		: 25  : Central Volume (L)
F1 		: 0.8 : Bioavailability (unitless)
LAGTIME : 0  : Lag Time (hr)

$OMEGA @annotated @block
ETA_KA      : 0.01 : ETA on KA
ETA_CL      : 0 0.01 : ETA on CL
ETA_V1      : 0 0 0.01 : ETA on V1
ETA_F1      : 0 0 0 0.01 : ETA on F1
ETA_LAGTIME : 0 0 0 0 0.01 : ETA on LAGTIME

$SIGMA @annotated @block
PK_ADD  : 0 : PK Additive Error
PK_PROP : 0 0.1 : PK Proportional Error

$CMT @annotated
GUT     : Extravascular (mass)
CENT    : Central (mass)

$MAIN
double KAi = KA * exp(ETA_KA);
double CLi = CL * exp(ETA_CL);
double V1i = V1 * exp(ETA_V1);
double F1i = F1 * exp(ETA_F1);
double LAGTIMEi = LAGTIME * exp(ETA_LAGTIME);

_F(1) = F1i;
_ALAG(1) = LAGTIMEi;

$ODE
dxdt_GUT = -KAi*GUT;
dxdt_CENT = KAi*GUT - (CLi/V1i)*CENT;

$TABLE
double CENTRAL = CENT/V1i * (1 + PK_PROP) + PK_ADD;

int i = 1;
while(CENTRAL < 0 & i <= 50) {
	simeps();
	CENTRAL = CENT/V1i * (1 + PK_PROP) + PK_ADD;
	i++;
}

if(i > 50) mrg::report("Positive concentrations could not be obtained.");

$CAPTURE @annotated
CENTRAL    : Plasma Concentration (mass/time)

