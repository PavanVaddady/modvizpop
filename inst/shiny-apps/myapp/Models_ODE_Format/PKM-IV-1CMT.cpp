$PROB IV ONE COMP PK MODEL

$PARAM @annotated
CL 		: 5   : Clearance (L/hr)
V1 		: 25  : Central Volume (L)

$OMEGA @annotated @block
ETA_CL      : 0.01 : ETA on CL
ETA_V1      : 0 0.01 : ETA on V1

$SIGMA @annotated @block
PK_ADD  : 0 : PK Additive Error
PK_PROP : 0 0.1 : PK Proportional Error

$CMT @annotated
CENT    : Central (mass)

$MAIN
double CLi = CL * exp(ETA_CL);
double V1i = V1 * exp(ETA_V1);

$ODE
dxdt_CENT = - (CLi/V1i)*CENT;

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
