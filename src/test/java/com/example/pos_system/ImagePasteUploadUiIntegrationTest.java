package com.example.pos_system;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class ImagePasteUploadUiIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void productFormRendersPasteUploadZone() throws Exception {
        mockMvc.perform(get("/products/new")
                        .with(SecurityMockMvcRequestPostProcessors.user("admin").roles("ADMIN")))
                .andExpect(status().isOk())
                .andExpect(content().string(Matchers.containsString("productImagePasteZone")))
                .andExpect(content().string(Matchers.containsString("Ctrl/Cmd + V")));
    }

    @Test
    void categoryFormRendersPasteUploadZone() throws Exception {
        mockMvc.perform(get("/categories/new")
                        .with(SecurityMockMvcRequestPostProcessors.user("admin").roles("ADMIN")))
                .andExpect(status().isOk())
                .andExpect(content().string(Matchers.containsString("categoryImagePasteZone")))
                .andExpect(content().string(Matchers.containsString("drag & drop")));
    }
}
