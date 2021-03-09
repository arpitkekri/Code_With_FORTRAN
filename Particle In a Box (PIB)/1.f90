program tisecode
    implicit none 
    
    integer i,j,n,nx,nspec,lwork,ifail
    real pi,xmax,xmin,dx,t,temp,hbar,m
    character u,v 
    real,dimension(:),ALLOCATABLE::pot,spec,diag,work
    real,dimension(:,:),ALLOCATABLE::h 
    
    pi=acos(-1.0)
    hbar=1.0
    m=1.0
    
    print*,"no of points"
    read*,nx
    
    xmin=0.0 
    xmax=5.0 
    dx=(xmax-xmin)/(nx-1.0)
    
    ALLOCATE(pot(nx),spec(nx),diag(nx),work(nx))
    ALLOCATE(h(nx,nx))
    t=hbar/(2.0*m*(dx**2))
    
    do i=1,nx
        pot(i)=0.0 
    enddo 
    
    do i=1,nx
        do j=1,nx 
            if (i==j) then 
                h(i,j)= 2.0*t+pot(i)
            elseif ((i-j==1).or.(j-i==1)) then 
                h(i,j)= -t
            endif 
        enddo 
    enddo 
    
    do i=1,5 
        !print*,(h(i,j),j=1,5)
    enddo 
    
    lwork=64
    CALL dsyev('v','u',nx,h,nx,diag,work,lwork,ifail)
    
    do i=1,nx
        do j=1,nx
            if (diag(i).gt.diag(j)) then 
                temp=diag(j)
                diag(j)=diag(i)
                diag(i)=temp
            endif 
        enddo
    enddo 
    
    h=h/(sqrt(dx))
    
    open(unit=10, file='output1.txt')
        n=5
        do i=1,5
        temp=0.0
        temp=((i**2)*(hbar**2)*(pi**2))/(2.0*m*((xmax-xmin)**2))
        write(20,*) diag(i),temp,h(i,n)
        enddo 
    close(10)
    
    print*,"expectation value of x for n=?"
    read*,nspec 
    spec=0.0 
    
    do i=1,nx 
        spec=spec+h(i,nspec)*xmin+dx*(i-1.0)*h(i,nspec)*dx
    enddo 
    
    lwork=64
    CALL dsyev('v','u',nx,h,nx,diag,work,lwork,ifail)
    
    DEALLOCATE(pot,spec,diag,work) 
    DEALLOCATE(h)
    
    end