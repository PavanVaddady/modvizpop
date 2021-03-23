$PROB ABS TWO COMP PKPD NEUTROPENIA MODEL

$PARAM @annotated
KA 		: 0.61 : Absorption Rate Constant (1/hr)
CL 		: 79   : Clearance (L/hr)
V2 		: 28   : Central Volume (L)
V3 		: 723  : Peripheral Volume (L)
Q 		: 41   : Intercompartmental Clearance (L/hr)
F1 		: 1 	: Bioavailability (unitless)
LAGTIME : 0.21 : Lag Time (hr)
KTR 	: 0.15     : Transit rate (1/hr)
KPROL 	: 0.15     : Transit rate (1/hr)
KCIRC 	: 0.15     : Transit rate (1/hr)
BASE 	: 5  	    : Baseline Neutrophil Count (*10^9/L)
GAM 	: 0.16     : Shape factor on Feedback loop
IMAX 	: 1   	    : Maximum Effect (units)
IC50 	: 0.5      : Emax Model Sigmoidicity 

$OMEGA @annotated @block
ETA_KA      : 0.01 : ETA on KA
ETA_CL      : 0 0.01 : ETA on CL
ETA_V2      : 0 0 0.01 : ETA on V2
ETA_V3      : 0 0 0 0.01 : ETA on V3
ETA_Q       : 0 0 0 0 0.01 : ETA on Q
ETA_F1      : 0 0 0 0 0 0.01 : ETA on F1
ETA_LAGTIME : 0 0 0 0 0 0 0.01 : ETA on LAGTIME 
ETA_KTR     : 0 0 0 0 0 0 0 0    : ETA on KTR
ETA_KPROL   : 0 0 0 0 0 0 0 0 0 	: ETA on KPROL
ETA_KCIRC   : 0 0 0 0 0 0 0 0 0 0	  : ETA on KCIRC
ETA_BASE    : 0 0 0 0 0 0 0 0 0 0 0.01 : ETA on BASE
ETA_GAM     : 0 0 0 0 0 0 0 0 0 0 0 0.01 : ETA on GAM
ETA_IMAX    : 0 0 0 0 0 0 0 0 0 0 0 0 0.01 : ETA on IMAX  
ETA_IC50    : 0 0 0 0 0 0 0 0 0 0 0 0 0 0.01 : ETA on IC50
  
$SIGMA @annotated @block   
PK_ADD  : 0 : PK Additive Error 
PK_PROP : 0 0.1 : PK Proportional Error 
PD_ADD  : 0 0 0 : PD Additive Error 
PD_PROP : 0 0 0 0.01 : PD Proportional Error    

$CMT @annotated
GUT     : Extravascular (mass)
CENT    : Central (mass)
PERIPH  : Peripheral (mass)
PROL    : Proliferative cells
TRANS1	: Transit CMT1
TRANS2	: Transit CMT2
TRANS3	: Transit CMT3	
CIRC	: Circulating Cells (ANC)

[GLOBAL]
#define CP (CENT/(V2))


$MAIN
double KAi = KA * exp(ETA_KA);
double CLi = CL * exp(ETA_CL);
double V2i = V2 * exp(ETA_V2);
double V3i = V3 * exp(ETA_V3);
double Qi = Q * exp(ETA_Q);
double F1i = F1 * exp(ETA_F1);
double LAGTIMEi = LAGTIME * exp(ETA_LAGTIME);

double KTRi    = KTR * exp(ETA_KTR);
double KPROLi  = KPROL * exp(ETA_KPROL);
double KCIRCi  = KCIRC * exp(ETA_KCIRC);
double BASEi   = BASE * exp(ETA_BASE);
double GAMi    = GAM * exp(ETA_GAM);
double IMAXi   = IMAX * exp(ETA_IMAX);
double IC50i   = IC50 * exp(ETA_IC50);


_F(1) = F1i;
_ALAG(1) = LAGTIMEi;

double NTR = 3;
double MTT = (NTR+1)/KTRi;


double k20 = CLi/V2i;
double k23 = Qi/V2i;
double k32 = Qi/V3i;

PROL_0  =BASEi;
TRANS1_0=BASEi;
TRANS2_0=BASEi;
TRANS3_0=BASEi;
CIRC_0  =BASEi;

$ODE
dxdt_GUT = -KAi*GUT;
dxdt_CENT = KAi*GUT - (k20+k23)*CENT + k32*PERIPH;
dxdt_PERIPH = k23*CENT - k32*PERIPH;

double DRUGEFF = (IMAXi*CP)/(IC50i+CP);
double FEEDBACK = pow((BASEi/CIRC),GAMi);

dxdt_PROL = KPROLi*PROL*(1-DRUGEFF)*FEEDBACK-KTRi*PROL;
dxdt_TRANS1 = KTRi*PROL - KTRi*TRANS1;
dxdt_TRANS2 = KTRi*TRANS1 - KTRi*TRANS2;
dxdt_TRANS3 = KTRi*TRANS2 - KTRi*TRANS3;
dxdt_CIRC = KTRi*TRANS3 - KCIRCi*CIRC;



$TABLE
double CENTRAL = CENT/V2i * (1 + PK_PROP) + PK_ADD;
double ANC = CIRC * (1 + PD_PROP) +  PD_ADD;

$CAPTURE @annotated
CENTRAL    : Plasma Concentration (mass/time)
ANC 	   : ANC count (*10^9/L)
  
