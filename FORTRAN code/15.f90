module sub1m
    contains
    subroutine sub1()
    print *, "In sub1"
    end subroutine sub1
end module sub1m
program demo
    use sub1m
    print *, "In main program"
    call sub1()
end program demo