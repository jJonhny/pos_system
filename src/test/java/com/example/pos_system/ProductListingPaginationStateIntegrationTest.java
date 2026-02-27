package com.example.pos_system;

import com.example.pos_system.entity.Product;
import com.example.pos_system.repository.ProductRepo;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class ProductListingPaginationStateIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ProductRepo productRepo;

    /**
     * Executes the deleteRedirectKeepsPagingSortingAndFilters operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the deleteRedirectKeepsPagingSortingAndFilters operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the deleteRedirectKeepsPagingSortingAndFilters operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void deleteRedirectKeepsPagingSortingAndFilters() throws Exception {
        Product product = createProduct("Delete Redirect " + UUID.randomUUID());

        mockMvc.perform(post("/products/{id}/delete", product.getId())
                        .param("q", "soap")
                        .param("active", "true")
                        .param("priceMin", "1.25")
                        .param("stockMin", "2")
                        .param("sort", "price")
                        .param("dir", "desc")
                        .param("page", "7")
                        .param("size", "50")
                        .with(SecurityMockMvcRequestPostProcessors.user("admin").roles("ADMIN"))
                        .with(csrf()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/products?q=soap&active=true&priceMin=1.25&stockMin=2&sort=price&dir=desc&page=7&size=50"));
    }

    /**
     * Executes the quickUpdateRedirectKeepsPagingSortingAndFilters operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the quickUpdateRedirectKeepsPagingSortingAndFilters operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the quickUpdateRedirectKeepsPagingSortingAndFilters operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void quickUpdateRedirectKeepsPagingSortingAndFilters() throws Exception {
        Product product = createProduct("Quick Redirect " + UUID.randomUUID());

        mockMvc.perform(post("/products/{id}/quick-update", product.getId())
                        .param("price", "9.99")
                        .param("stockQty", "8")
                        .param("lowStock", "true")
                        .param("q", "coffee")
                        .param("active", "true")
                        .param("sort", "sku")
                        .param("dir", "desc")
                        .param("page", "10")
                        .param("size", "20")
                        .with(SecurityMockMvcRequestPostProcessors.user("admin").roles("ADMIN"))
                        .with(csrf()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/products?lowStock=true&q=coffee&active=true&sort=sku&dir=desc&page=10&size=20"));
    }

    /**
     * Executes the importRedirectKeepsPagingSortingAndFiltersWhenFileMissing operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the importRedirectKeepsPagingSortingAndFiltersWhenFileMissing operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the importRedirectKeepsPagingSortingAndFiltersWhenFileMissing operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void importRedirectKeepsPagingSortingAndFiltersWhenFileMissing() throws Exception {
        MockMultipartFile emptyFile = new MockMultipartFile("file", "", "text/csv", new byte[0]);

        mockMvc.perform(multipart("/products/import")
                        .file(emptyFile)
                        .param("q", "water")
                        .param("sort", "stock")
                        .param("dir", "asc")
                        .param("page", "9")
                        .param("size", "100")
                        .with(SecurityMockMvcRequestPostProcessors.user("admin").roles("ADMIN"))
                        .with(csrf()))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/products?q=water&sort=stock&dir=asc&page=9&size=100"));
    }

    /**
     * Executes the outOfRangePageRedirectsToLastPageInsteadOfPageOne operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the outOfRangePageRedirectsToLastPageInsteadOfPageOne operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the outOfRangePageRedirectsToLastPageInsteadOfPageOne operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void outOfRangePageRedirectsToLastPageInsteadOfPageOne() throws Exception {
        String marker = "Marker-" + UUID.randomUUID();
        createProduct(marker + "-1");
        createProduct(marker + "-2");
        createProduct(marker + "-3");

        mockMvc.perform(get("/products")
                        .param("q", marker)
                        .param("sort", "name")
                        .param("dir", "asc")
                        .param("page", "10")
                        .param("size", "2")
                        .with(SecurityMockMvcRequestPostProcessors.user("admin").roles("ADMIN")))
                .andExpect(status().is3xxRedirection())
                .andExpect(redirectedUrl("/products?q=" + marker + "&sort=name&dir=asc&page=1&size=2"));
    }

    /**
     * Executes the createProductStartsStockAtZeroEvenWhenStockSubmitted operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createProductStartsStockAtZeroEvenWhenStockSubmitted operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    /**
     * Executes the createProductStartsStockAtZeroEvenWhenStockSubmitted operation.
     *
     * @return void No value is returned; the method applies side effects to existing state.
     * @throws Exception If the operation cannot complete successfully.
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    @Test
    void createProductStartsStockAtZeroEvenWhenStockSubmitted() throws Exception {
        String sku = "ZERO-" + UUID.randomUUID();
        mockMvc.perform(post("/products")
                        .param("name", "Zero Stock Product " + UUID.randomUUID())
                        .param("sku", sku)
                        .param("price", "15.50")
                        .param("stockQty", "99")
                        .with(SecurityMockMvcRequestPostProcessors.user("admin").roles("ADMIN"))
                        .with(csrf()))
                .andExpect(status().is3xxRedirection());

        Product created = productRepo.findBySkuIgnoreCase(sku).orElseThrow();
        assertThat(created.getStockQty()).isEqualTo(0);
    }

    /**
     * Executes the createProduct operation.
     *
     * @param name Parameter of type {@code String} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Product createProduct(String name) {
        Product product = new Product();
        product.setName(name);
        product.setPrice(BigDecimal.valueOf(10));
        product.setStockQty(5);
        product.setAllowNegativeStock(false);
        product.setActive(true);
        return productRepo.save(product);
    }
}
