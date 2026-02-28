package com.devcore.pos_system;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
public class PosSystemApplication {

	/**
	 * Executes the main operation.
	 *
	 * @param args Parameter of type {@code String[]} used by this operation.
	 * @return void No value is returned; the method applies side effects to existing state.
	 * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
	 * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
	 */
	public static void main(String[] args) {
		SpringApplication.run(PosSystemApplication.class, args);
	}

}
