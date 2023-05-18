// Renaming map module
// While you are free to structure your implementation however you
// like, you are advised to only add code to the TODO sections
module renaming_map import ariane_pkg::*; #(
    parameter int unsigned ARCH_REG_WIDTH = 5,
    parameter int unsigned PHYS_REG_WIDTH = 6
)(
    // Clock and reset signals
    input logic clk_i,
    input logic rst_ni,

    // Indicator that there is a new instruction to rename
    input logic fetch_entry_ready_i,

    // Input decoded instruction entry from the ID stage
    input issue_struct_t issue_n,

    // Output instruction entry with registers renamed
    output issue_struct_t issue_q,

    // Destination register of the committing instruction
    input logic [PHYS_REG_WIDTH-1:0] waddr_i,
    
    // Indicator signal that there is a new committing instruction
    input logic we_gp_i
);

    // 32 architectural registers and 64 physical registers
    localparam ARCH_NUM_REGS = 2**ARCH_REG_WIDTH;
    localparam PHYS_NUM_REGS = 2**PHYS_REG_WIDTH;

    logic [PHYS_REG_WIDTH-1:0] rs1;
    logic [PHYS_REG_WIDTH-1:0] rs2;
    logic [PHYS_REG_WIDTH-1:0] rd;

    // TODO: ADD STRUCTURES TO EXECUTE REGISTER RENAMING
    
    // There'll be one entry per architectural register.
    typedef struct packed {
        logic                      valid;   // Valid bit
        logic [PHYS_REG_WIDTH-1:0] pr;      // Physical register
    } rename_table_entry_t;

    // Maps architectural registers to physical registers.
    rename_table_entry_t rename_table[ARCH_NUM_REGS];

    // Initialize the rename table to only map ar0 to pr0.
    initial begin
        rename_table[0].valid = 1;
        rename_table[0].pr = 0;
        for (int i = 1; i < ARCH_NUM_REGS; i++) begin
            rename_table[i].valid = 0;
        end
    end

    // Maps physical registers to whether or not they're free.
    //  0: free
    //  1: allocated
    // Initially, pr0 is the only physical register allocated.
    logic [PHYS_NUM_REGS-1:0] pr_free_map = '1;

    typedef struct packed {
        logic                      valid;   // Valid bit
        logic [PHYS_REG_WIDTH-1:0] pr;      // Physical register
    } deallocation_map_entry_t;

    // The deallocation map tells us: when a physical register is the destination
    // of the committing instruction (i.e., it's currently being written with the
    // result of a compution), whether another physical register should now be
    // deallocated.
    //
    // When the destination register of an instruction already has a mapping in
    // the rename map, we *can't* deallocate its current associated physical
    // register during instruction decode (ID) because there might still be
    // instructions in execution that reference the register. However, when the
    // instruction *commits*, because commits happen in program order, we know
    // for sure that no instructions in execution reference the old register
    // because all subsequent instructions use the new physical register mapping.
    deallocation_map_entry_t deallocation_map[PHYS_NUM_REGS];

    // Initialize the deallocation map.
    initial begin
        for (int i = 0; i < PHYS_NUM_REGS; i++) begin
            deallocation_map[i].valid = 0;
        end
    end

    // Positive clock edge used for renaming new instructions
    always @(posedge clk_i, negedge rst_ni) begin
        // Processor reset: revert renaming state to reset conditions    
        if (~rst_ni) begin

            // TODO: ADD LOGIC TO RESET RENAMING STATE
            
            // Clear out the rename table.
            for (int i = 1; i < ARCH_NUM_REGS; i++) begin
                rename_table[i].valid = 0;
            end

            // Mark all physical registers (except pr0) as free again.
            for (int i = 1; i < PHYS_NUM_REGS; i++) begin
                pr_free_map[i] = 0;
            end

            // Clearing the deallocation map.
            for (int i = 0; i < PHYS_NUM_REGS; i++) begin
                deallocation_map[i].valid = 0;
            end
    
        // New incoming valid instruction to rename   
        end else if (fetch_entry_ready_i && issue_n.valid) begin
            // Get values of registers in new instruction
            rs1 = issue_n.sbe.rs1[PHYS_REG_WIDTH-1:0];
            rs2 = issue_n.sbe.rs2[PHYS_REG_WIDTH-1:0];
            rd = issue_n.sbe.rd[PHYS_REG_WIDTH-1:0];

            // Set outgoing instruction to incoming instruction without
            // renaming by default. Keep this line since all fields of the 
            // incoming issue_struct_t should carry over to the output
            // except for the register values, which you may rename below
            issue_q = issue_n;

            // TODO: ADD LOGIC TO RENAME OUTGOING INSTRUCTION
            
            issue_q.sbe.rs1[PHYS_REG_WIDTH-1:0] = rename_table[rs1].valid ? rename_table[rs1].pr : 0;
            issue_q.sbe.rs2[PHYS_REG_WIDTH-1:0] = rename_table[rs2].valid ? rename_table[rs2].pr : 0;
            if (rd == 0) begin
                // Special case: no need to allocate a new physical register.
                issue_q.sbe.rd[PHYS_REG_WIDTH-1:0] = 0;
            end else begin
                // Allocate the lowest-numbered free physical register.
                assert (~pr_free_map != 0) else $error("No physical registers available!");
                for (int i = 1; i < PHYS_NUM_REGS; i++) begin
                    if (pr_free_map[i] == 0) begin
                        pr_free_map[i] = 1;

                        if (rename_table[rd].valid) begin
                            // Architectural register currently mapped.
                            deallocation_map[i].valid = 1;
                            deallocation_map[i].pr = rename_table[rd].pr;
                        end

                        rename_table[rd].valid = 1;
                        rename_table[rd].pr = i;

                        issue_q.sbe.rd[PHYS_REG_WIDTH-1:0] = i;
                        break;
                    end
                end
            end
    
        // If there is no new instruction this clock cycle, simply pass on the
        // incoming instruction without renaming
        end else begin
            issue_q = issue_n;
        end
    end
    

    // Negative clock edge used for physical register deallocation 
    always @(negedge clk_i) begin
        if (rst_ni) begin
            // If there is a new committing instruction and its prd is not pr0,
            // execute register deallocation logic to reuse physical registers
            if (we_gp_i && waddr_i != 0) begin
        
                // TODO: IMPLEMENT REGISTER DEALLOCATION LOGIC
                if (deallocation_map[waddr_i].valid) begin
                    pr_free_map[deallocation_map[waddr_i].pr] = 0;
                    deallocation_map[waddr_i].valid = 0;
                end

            end
        end
    end
endmodule
