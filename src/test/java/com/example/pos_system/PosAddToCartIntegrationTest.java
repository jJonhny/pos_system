package com.example.pos_system;

import com.example.pos_system.entity.Category;
import com.example.pos_system.entity.Product;
import com.example.pos_system.repository.CategoryRepo;
import com.example.pos_system.repository.ProductRepo;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
class PosAddToCartIntegrationTest {
    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private CategoryRepo categoryRepo;
    @Autowired
    private ProductRepo productRepo;

    @Test
    void addToCartAddsLineItemForCashier() throws Exception {
        Product product = createProduct();
        var result = mockMvc.perform(post("/pos/cart/add/{id}", product.getId())
                        .header("HX-Request", "true")
                        .with(SecurityMockMvcRequestPostProcessors.user("cashier").roles("CASHIER"))
                        .with(SecurityMockMvcRequestPostProcessors.csrf()))
                .andExpect(status().isOk())
                .andExpect(content().string(org.hamcrest.Matchers.containsString("Cart")))
                .andReturn();

        Object cartObject = result.getRequest().getSession().getAttribute("cart");
        assertThat(cartObject).isNotNull();
    }

    private Product createProduct() {
        Category category = new Category();
        category.setName("POS Test Category");
        category.setActive(true);
        category.setSortOrder(1);
        Category savedCategory = categoryRepo.save(category);

        Product product = new Product();
        product.setName("POS Test Product");
        product.setSku("POS-ADD-001");
        product.setPrice(new BigDecimal("3.50"));
        product.setStockQty(30);
        product.setActive(true);
        product.setCategory(savedCategory);
        return productRepo.save(product);
    }
}
