PROGRAM calculate_cost
    IMPLICIT NONE

    integer :: item
    real :: discount, cost_per_item, total_cost

    write(*, *) "Enter number of items and cost of one item"
    read (*, *) item, cost_per_item;

    if(item >= 10) then  
        discount = 0.25
    else if((item > 5) .and. (item <= 10)) then
        discount = 0.15
    else if((item > 1) .and. (item <= 5)) then
        discount = 0.01
    else 
        discount = 0.0
    end if 
    total_cost = cost_per_item * item
    total_cost = total_cost - total_cost * discount
    write(*, *) total_cost

END