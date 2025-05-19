library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


--Fs = 1.56MHz; Fpass=37.5Khz, Fstop = 100KHz; Astop = 60dB; Apass = 1dB
entity IIR_Filter_ChbyChevII_Fc37K5_Fp100K_Astop60dB is
     Port (
         clk                  : in  std_logic := '0';           -- Clock signal
         reset                : in  std_logic := '0';           -- Reset signal
         x_in                 : in  signed (15 downto 0):= (others=>'0');    
   
         iir_out              : out signed(15 downto 0):= (others=>'0');  -- Filtered output
         sample_valid_out     : out std_logic := '0';     -- Sample valid output signal
         busy                 : out std_logic := '0'      -- Busy signal
     );
end IIR_Filter_ChbyChevII_Fc37K5_Fp100K_Astop60dB;

architecture Behavioral of IIR_Filter_ChbyChevII_Fc37K5_Fp100K_Astop60dB is
 
     -- Register declarations for input (x) and output (y) values
     type x_regArray is array(0 to 6) of signed(31 downto 0); 
     signal x_reg : x_regArray; 
     type y_regArray is array(0 to 6) of signed(31 downto 0);  
     signal y_reg : y_regArray; 

     -- Multiplication signals
     type mul_xArray  is array(0 to 6) of signed(63 downto 0); 
     signal mul_x : mul_xArray; 
     type mul_yArray is array(1 to 6) of signed(63 downto 0);  
     signal mul_y : mul_yArray; 
   
     -- Sum signals
     type Sum_xArray is array(0 to 6) of signed(63 downto 0);  
     signal Sum_x : Sum_xArray;  
     type Sum_yArray  is array(0 to 6) of signed(63 downto 0); 
     signal Sum_y : Sum_yArray; 
    
     -- Coefficients (b and a arrays)
     type b_Coefficients is array(0 to 6) of signed(31 downto 0);
     type a_Coefficients is array(0 to 6) of signed(31 downto 0);
   
     signal x_in_sig: signed(15 downto 0); 
    
     constant b : b_Coefficients := (    
         to_signed(20004, 32),
         to_signed(-79756, 32),
         to_signed(154737, 32),
         to_signed(-188354, 32),
         to_signed(154737, 32),
         to_signed(-79756, 32),
         to_signed(20004, 32)       
    );

     constant a : a_Coefficients := (     
         to_signed(16777216, 32),
         to_signed(-86221058, 32),
         to_signed(185542407, 32),
         to_signed(-213901648, 32),
         to_signed(139278978, 32),
         to_signed(-48550822, 32),
         to_signed(7076543, 32)
    );
    
     -- Final output signal
     signal Output : signed(31 downto 0):= (others=>'0');  
    
     signal SumDiff : signed(63 downto 0):= (others=>'0'); 

     -- State machine state
     signal state : integer := 0;
     signal sample_valid_in_sig : std_logic:='0'; 
     
     COMPONENT  Noisy_Signal_Generation 
     Port (
         clk                  : in  std_logic := '0';           -- Clock signal
         reset                : in  std_logic := '0';           -- Reset signal
         x_out                : Out signed(15 downto 0):= (others=>'0');   
         sample_valid_out     : out std_logic := '0'      -- Sample valid input signal
     );
     END COMPONENT;
 
begin

SineWaveGen : Noisy_Signal_Generation
     PORT MAP (
		 clk  => clk,                
		 reset => reset,             
		 x_out => x_in_sig,                  
		 sample_valid_out => sample_valid_in_sig
  ); 
    -- Process for updating the filter with pipelined calculations
process(clk, reset)
     begin
         if reset = '1' then
             -- Reset input and output registers to 0
             x_reg <= (others => (others => '0'));
             y_reg <= (others => (others => '0'));
            
             mul_x <= (others => (others => '0'));
             mul_y <= (others => (others => '0'));
            
             Sum_x <= (others => (others => '0'));
             Sum_y <= (others => (others => '0'));
            
             sample_valid_out <= '0';
             busy <= '0';
             state <= 0;
        
         elsif rising_edge(clk) then
             case state is

                 -- Initial stage: Load the input values and shift registers
                 when 0 =>
                     if sample_valid_in_sig = '1'  then
                         x_reg(0) <= resize(x_in_sig,32); 
                         state <= 1;
                         busy <= '1';
                     end if;

                -- Stage 1: Multiply input values by coefficients
                when 1 =>

                     for i in 0 to 6 loop
                         mul_x(i) <= x_reg(i) * b(i);  --8 bits * 16 bits  ==> need 24 bits
                     end loop;

                     -- Multiply previous output by feedback coefficients
                     for i in 1 to 6 loop
                         mul_y(i) <= y_reg(i) * a(i);  --8 bits * 16 bits  ==> need 24 bits
                     end loop;

                    state <= 2;

                 -- Stage 2: Sum the multiplications
                 when 2 =>
                     Sum_x(0) <= resize(mul_x(0) + mul_x(1),64);  
                     Sum_x(1) <= resize(mul_x(2) + mul_x(3),64);  
                     Sum_x(2) <= resize(mul_x(4) + mul_x(5),64);
                                                                 
                         
                     Sum_y(0) <= resize(mul_y(1) + mul_y(2),64);   
                     Sum_y(1) <= resize(mul_y(3) + mul_y(4),64); 
                     Sum_y(2) <= resize(mul_y(5) + mul_y(6),64);   

                     state <= 3;
                    
                 when 3 =>
                     Sum_x(3) <= resize(Sum_x(0) + Sum_x(1),64);    
                     Sum_x(4) <= resize(Sum_x(2) + mul_x(6),64);        
                                                                    
                     Sum_y(3) <= resize(Sum_y(0) + Sum_y(1),64);
                    
                     state <= 4; 
                    
                 when 4 =>
                
                     Sum_x(5) <= resize(Sum_x(3) + Sum_x(4),64);             
                                                                             
                     Sum_y(4) <= resize(Sum_y(3) + Sum_y(2),64);               
                                                                            
                     state <= 5;
                    
                 when 5 =>
  
                     SumDiff <= Sum_x(5)- Sum_y(4);
                   
                   
                 state <= 6;
                    
                 when 6 => 
                    
                     Output <= resize(shift_right(SumDiff,24),32);   --scale down by 2^24
                   
                     state <= 7;
                    
                 when 7 =>   
                   
                     y_reg(1)<=  Output;
                   
                     for i in 1 to 6 loop
                            x_reg(i) <= x_reg(i-1);
                         end loop;
              
                         for i in 2 to 6 loop
                            y_reg(i) <= y_reg(i-1);
                         end loop;
                    
                     sample_valid_out <= '1';
                    
                     state <= 8;

                     when 8 => 
                     
                     sample_valid_out <= '0';
                     busy <= '0';
                     state <= 0;
                 
                 when others =>
                     state <= 0;
             end case;
         end if;
     end process;


    iir_out <= Output(15 downto 0) when (Output >= -32768) AND (Output <= 32767) else
             to_signed(-32768,16) when (Output < -32768) else
             to_signed(32767,16) when (Output < 32767);


end Behavioral;
