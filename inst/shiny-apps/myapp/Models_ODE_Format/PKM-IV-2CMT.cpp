$PROB IV TWO COMP PK MODEL

$PARAM @annotated
CL 		: 5   : Clearance (L/hr)
Q 		: 2.5 : Intercompartmental Clearance (L/hr)
V1 		: 25  : Central Volume (L)
V2 		: 50  : Peripheral Volume (L)

$OMEGA @annotated @block
ETA_CL      : 0.01 : ETA on CL
ETA_Q       : 0 0.01 : ETA on Q
ETA_V1      : 0 0 0.01 : ETA on V1
ETA_V2      : 0 0 0 0.01 : ETA on V2

$SIGMA @annotated @block
PK_ADD  : 0 : PK Additive Error
PK_PROP : 0 0.1 : PK Proportional Error

$CMT @annotated
CENT    : Central (mass)
PERIPH  : Peripheral (mass)

$MAIN
double CLi = CL * exp(ETA_CL);
double Qi = Q * exp(ETA_Q);
double V1i = V1 * exp(ETA_V1);
double V2i = V2 * exp(ETA_V2);

$ODE
dxdt_CENT = - (CLi/V1i)*CENT - (Qi/V1i)*CENT + (Qi/V2i)*PERIPH;
dxdt_PERIPH = (Qi/V1i)*CENT - (Qi/V2i)*PERIPH;

$TABLE
double CENTRAL = CENT/V1i * (1 + PK_PROP) + PK_ADD;
double PERIPHERAL = PERIPH/V2i * (1 + PK_PROP) + PK_ADD;

int i = 1;
while(CENTRAL < 0 & i <= 50) {
	simeps();
	CENTRAL = CENT/V1i * (1 + PK_PROP) + PK_ADD;
	i++;
}

if(i > 50) mrg::report("Positive concentrations could not be obtained.");

$CAPTURE @annotated
CENTRAL    : Plasma Concentration (mass/time)
PERIPHERAL : Conc in Peripheral CMT (mass/time)
