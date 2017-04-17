def task5_pipeline():

    from grid_class import grid_class

    grid = grid_class()
    grid.set_actions_grid()

    print "Initial State : "
    grid.visualize()

    convergence_status = False
    counter = 0
    while not convergence_status:
        counter += 1
        convergence_status = grid.update_utility_grid()

    print "Final Expected Utilities + Resulting Policy : "
    grid.visualize()

    grid.generate_latex_table()

    print "OK"