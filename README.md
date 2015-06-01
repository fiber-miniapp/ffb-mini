FFB-MINI
========

* version: 1.0.1 (based on FFB 8.1)
* date: 2015/06/02
* contact: miniapp@riken.jp


About FFB-MINI and FrontFlow/blue
---------------------------------

This mini app, FFB-MINI, is based on a general purpose thermal flow simulation
program FrontFlow/blue (FFB), being developed by Institute of Industrial
Science, the University of Tokyo. FFB solves the unsteady incompressible
Navier-Stokes equations by finite element method. FFB is MPI parallelized with
domain decomposition. Although not using thread based parallelism explicitly,
the current version of FFB includes optimizations for the K computer with the
automatic thread parallelization functionality.

To refer to the original FFB application:

   * [Thermo-Fluid Analysis Solvers for Large-Scale Assembly](http://www.ciss.iis.u-tokyo.ac.jp/riss/english/project/fluid/)
   * [Introduction and Download (in Japanese)](http://www.ciss.iis.u-tokyo.ac.jp/software/)

Contact point of the original FFB application:
          Yoshinobu Yamade <yyamade@iis.u-tokyo.ac.jp>


Compilation
-----------

To build and run the program, Fortran90 and C compilers, an MPI library and GNU 
Make are required. To utilize the automatic parallelization with the Fujitsu 
compiler for the K computer and Fujitsu FX10 system, 
[METIS library](http://glaros.dtc.umn.edu/gkhome/views/metis) is required.
For calculations using automatic mesh-refinement (REFINER),
[REVOCAP_Refiner library](http://www.ciss.iis.u-tokyo.ac.jp/dl/index.php?pScdownload_6) is required.

1. Obtain the package and extract its contents.

2. Go to the src directory and edit the file "make_setting" according to your
   compilers and libraries. There are several examples:
    * make_setting.intel : For using Intel compilers
    * make_setting.gcc   : For using GCC compilers
    * make_setting.fx10  : For K
    * make_setting.fx10  : For Fujitsu FX10

3. Run make command in the src directory.
   After successful compilation, an executable file named "les3x.mpi" and its
   wrapper program "ffb_mini" are generated in the src directory.


Test
----

Test shell scripts are provided in the test directory. To run the test
interactively, simply run "make test" in the src or test directory.
("make test_k" for the K computer, "make test_fx10" for the FX10 system at
the University of Tokyo)

These test scripts runs the program ffb_mini with 8 MPI processes,
and compares the computed results with reference data,
then judges whether the calculation is correctly completed or not.


What FFB-MINI does -- brief explanation
---------------------------------------

This mini app is based on a fluid simulation code les3x.mpi included in the
FFB package. The basic loop structure of les3x.mpi is as below:

    time-step-loop {

        # solve velocity fields (U,V,W) by Crank-Nicolson scheme
        U-loop {
            # BiCGSTAB iteration
        }
        V-loop {
            # BiCGSTAB iteration
        }
        W-loop {
            # BiCGSTAB iteration
        }

        # solve pressure Poisson eq.
        RCM-outer-loop {
            RCM-inner-loop {
                # BiCGSTAB iteration
            }
        }

        # velocity filed correction

    }


To solve the velocity field equations formulated by Crank-Nicolson scheme,
BiCGSTAB is used for each of directions.

The pressure Poisson equation is solved by Residual Cutting Method (RCM).
BiCGSTAB with the iteration number fixed to 10 is used as the inner-loop-solver
of RCM.

### ffb_mini wrapper

When a wrapper program ffb_mini starts, first it makes input data for les3x.mpi,
then runs les3x.mpi internally. ffb_mini calculates 3D cavity flow problems,
in which the computational domain is a rectangular region surrounded by fixed
walls, except for "+Y wall" moving X direction.

Notes that les3x.mp and ffb_mini always perform floating point operations in
single-precision.


How to run the programs
-----------------------

### les3x.mpi

To run les3x.mpi, please refer to the original FFB manual "doc/Manual_FrontFlow_blue.8.1_ja.pdf".

### ffb_mini

For ffb_mini, all the necessary parameters should be given as the command line
options:

    > ffb_mini Npx Npy Npz N [key1=val1 key2=val2 ...]

where `Npx`, `Npy`, `Npz` and `N` are mandatory. Mesh size and how to decompose
the domain to MPI processes are specified by these four parameters in a weak
scaling fashion. ffb_mini solves the 3D cavity flow in a rectangular region
composed of `Npx`x`Npy`x`Npz` cubes. Each cube has `N`^3 hexahedral elements and
is assigned to an MPI process.

`key1=val1`, `key2=val2`, ..., are optional.
The list of optional parameters are as below:

    ntime=int   computing time steps (default 100)
    isolp=int　 type of the Poisson eq. solver, 1 (BiCGSTAB) or 2 (RCM)
                (default 2)
    dt=float    time step delta (default 0.005)
    nmaxp=int   max. number of the Poisson iterations (total number of RCM inner
                loop iterations) (default 50)
    nmaxt=int   max. number of the velocity fields iterations (default 5)
    epsp=float  Poisson iteration convergence tolerance
                (if epsp=0.0, iterates nmaxp times irrespective of the
                convergence) (default 0.0)
    epst=float  velocity fields iteration convergence tolerance
                (if epst=0.0, iterates nmaxt times irrespective of the
                convergence) (default 0.0)
    color_partsize=int
                size of coloring regions (if color_partsize=0, no coloring)
                (default 0)
    reorder_ndiv=int
                size of the block used for node number reordering
                (if reorder_ndiv=0, no reordering) (default 0)
    unroll=str  loop unrolling flog for matrix-vector multiplication in
                subroutine CALAX3
                on (full unrolling) or off (no unrolling) (default off)

For the K computer and Fujitsu FX10 system, we recommend following options:

    color_partsize=2000 reorder_ndiv=10 unroll=on

### Output files

Wrapper program ffb_mini outputs following files:

* MESH.PXXXX      ... mesh data
* BOUN.PXXXX      ... boundary condition data
* PARMLES3X.PXXXX ... runtime parameters
* FLOW.PXXXX      ... calculation result (flow data)
* les3x.log.PXXXX ... runtime logs

where "XXXX" in suffix PXXXX describes an integer between 1 and Np, the number 
of MPI processes. For the rank 0 process, runtime logs are sent to stdout 
instead of les3x.log.P0001.

Notes that MESH.PXXXX, BOUN.PXXXX and PARMLES3X.PXXXX are used as input data to
les3x.mpi, which runs internally in ffb_mini.


Target exa-scale problem setting
--------------------------------

The current model setting targeted at exa-scale simulations is as follows:

  - 10^12 elements system size
  - less than 100-hours real time with 4x10^4 simulation time steps
    (about 10sec/step)


Major changes from the original FFB 8.1
---------------------------------------

* Delete all programs other than les3x.mpi

* Reduce code size by limiting the functionality:

    - restrict to unsteady flow calculations using hexahedral element mesh in
      pressure-defined-on-element mode
    - delete following functionality: Large Eddy Simulations (LES), cavitation
      model, Volume of Fluid (VOF), overset and multi-frame, mid-node,
      Arbitrary Lagrangian and Eulerian Method (ALE), fan model and porous model
    - delete sampling output and history output

* Remove METIS and REVOCAP_Refiner libraries

* Add a wrapper program ffb_mini

* Add test programs

* Other code clean-up


Further detail information
--------------------------

### Hot spots

There are three major hot spot subroutines in the mini app version of les3x.mpi.

   * GRAD3X: calculates velocity gradient
   * FLD3X2: calculates divergence of velocity gradient
   * CALAX3: calculates matrix-vector multiplications

GRAD3X and FLD3X2 are called from subroutine CALLAP, which calculates the
Laplacian to solve the pressure Poisson equation. CALAX3 is called in the
velocity fields BiCGSTAB loop (RCM-inner-loop).

The call counts of the hot spot subroutines for one time step iteration are:

   * GRAD3X: 2.4 * NMAXP + 1
   * FLD3X2: 2.4 * NMAXP + 1
   * CALAX3: 6 * NMAXT + 3

where NMAXP and NMAXT are the number of the pressure Poisson loop iterations
(total iteration number of RCM-inner loop) and the number of the velocity field
BiCGSTAB iterations, respectively.

### MPI Communications

There are two major parts of the inter-process communications in the time step
loop:

   * Superimposing the data on the nodes located simultaneously on the
     neighboring sub-domains, in subroutine DDCMX
   * All-reduce communications for the global inner products, in subroutine
     DDCOM2

In subroutine RCMELM, which solves the pressure Poisson equation by RCM,
the call counts of subroutine DDCMX and DDCOM2 for one time step iteration are

   * DDCOMX: 2.4 * NMAXP + 1
   * DDCOM2: 5.5 * NMAXP + 2

In subroutine VEL3D1, which solve velocity fields by BiCGSTAB,

   * DDCOMX: 6 * NMAXT + 3
   * DDCOM2: 15 * NMAXT + 6  


References
----------

* Minami et al., "Performance Tuning and Evaluation of Sparse matrix-vector
  multiplication on the K computer",
  IPSJ High Performance Computing Symposium, 2012. pp.23-31, 2012. (in Japanese)
* Kumahata et al., "Kernel performance improvement for the FEM-based fluid
  analysis code on the K computer",
  Procedia Computer Science Vol.18, pp.2496-2499, 2013.
* Kumahata et al.,"Performance Tuning for Gradient Kernel of the
  FrontFlow/blue on the K computer",
  IPSJ Transactions on Advanced Computing Systems Vol.6, No.3 pp.31-42, 2013.
  (in Japanese)


Version History
---------------

* 1.0.0 (May 21, 2015)
    - Inital release

* 1.0.1 (June 2, 2015)
    - Fix a bug in makemesh

