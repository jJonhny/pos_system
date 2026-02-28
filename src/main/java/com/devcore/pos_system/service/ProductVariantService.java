package com.devcore.pos_system.service;

import com.devcore.pos_system.dto.VariantApiDtos;
import com.devcore.pos_system.entity.AttributeGroup;
import com.devcore.pos_system.entity.AttributeValue;
import com.devcore.pos_system.entity.Product;
import com.devcore.pos_system.entity.ProductAttributeGroup;
import com.devcore.pos_system.entity.ProductAttributeValue;
import com.devcore.pos_system.entity.ProductVariant;
import com.devcore.pos_system.entity.ProductVariantAttribute;
import com.devcore.pos_system.entity.ProductVariantExclusion;
import com.devcore.pos_system.repository.AttributeGroupRepo;
import com.devcore.pos_system.repository.AttributeValueRepo;
import com.devcore.pos_system.repository.ProductAttributeGroupRepo;
import com.devcore.pos_system.repository.ProductAttributeValueRepo;
import com.devcore.pos_system.repository.ProductRepo;
import com.devcore.pos_system.repository.ProductVariantAttributeRepo;
import com.devcore.pos_system.repository.ProductVariantExclusionRepo;
import com.devcore.pos_system.repository.ProductVariantRepo;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
public class ProductVariantService {
    private static final int DEFAULT_MAX_VARIANTS = 2000;

    private final ProductRepo productRepo;
    private final AttributeGroupRepo attributeGroupRepo;
    private final AttributeValueRepo attributeValueRepo;
    private final ProductAttributeGroupRepo productAttributeGroupRepo;
    private final ProductAttributeValueRepo productAttributeValueRepo;
    private final ProductVariantRepo productVariantRepo;
    private final ProductVariantAttributeRepo productVariantAttributeRepo;
    private final ProductVariantExclusionRepo productVariantExclusionRepo;
    private final VariantCombinationKeyService keyService;

    /**
     * Executes the ProductVariantService operation.
     * <p>Return value: A fully initialized ProductVariantService instance.</p>
     *
     * @param productRepo Parameter of type {@code ProductRepo} used by this operation.
     * @param attributeGroupRepo Parameter of type {@code AttributeGroupRepo} used by this operation.
     * @param attributeValueRepo Parameter of type {@code AttributeValueRepo} used by this operation.
     * @param productAttributeGroupRepo Parameter of type {@code ProductAttributeGroupRepo} used by this operation.
     * @param productAttributeValueRepo Parameter of type {@code ProductAttributeValueRepo} used by this operation.
     * @param productVariantRepo Parameter of type {@code ProductVariantRepo} used by this operation.
     * @param productVariantAttributeRepo Parameter of type {@code ProductVariantAttributeRepo} used by this operation.
     * @param productVariantExclusionRepo Parameter of type {@code ProductVariantExclusionRepo} used by this operation.
     * @param keyService Parameter of type {@code VariantCombinationKeyService} used by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ProductVariantService(ProductRepo productRepo,
                                 AttributeGroupRepo attributeGroupRepo,
                                 AttributeValueRepo attributeValueRepo,
                                 ProductAttributeGroupRepo productAttributeGroupRepo,
                                 ProductAttributeValueRepo productAttributeValueRepo,
                                 ProductVariantRepo productVariantRepo,
                                 ProductVariantAttributeRepo productVariantAttributeRepo,
                                 ProductVariantExclusionRepo productVariantExclusionRepo,
                                 VariantCombinationKeyService keyService) {
        this.productRepo = productRepo;
        this.attributeGroupRepo = attributeGroupRepo;
        this.attributeValueRepo = attributeValueRepo;
        this.productAttributeGroupRepo = productAttributeGroupRepo;
        this.productAttributeValueRepo = productAttributeValueRepo;
        this.productVariantRepo = productVariantRepo;
        this.productVariantAttributeRepo = productVariantAttributeRepo;
        this.productVariantExclusionRepo = productVariantExclusionRepo;
        this.keyService = keyService;
    }

    /**
     * Executes the createAttributeGroup operation.
     *
     * @param request Parameter of type {@code VariantApiDtos.AttributeGroupCreateRequest} used by this operation.
     * @return {@code AttributeGroup} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public AttributeGroup createAttributeGroup(VariantApiDtos.AttributeGroupCreateRequest request) {
        String code = normalizeCode(request == null ? null : request.code());
        String name = normalizeText(request == null ? null : request.name());
        if (code == null || name == null) {
            throw new IllegalArgumentException("Attribute group code and name are required.");
        }
        if (attributeGroupRepo.findByCodeIgnoreCase(code).isPresent()) {
            throw new IllegalArgumentException("Attribute group code already exists.");
        }
        AttributeGroup group = new AttributeGroup();
        group.setCode(code);
        group.setName(name);
        group.setSortOrder(request != null && request.sortOrder() != null ? request.sortOrder() : 0);
        group.setActive(true);
        return attributeGroupRepo.save(group);
    }

    /**
     * Executes the createAttributeValue operation.
     *
     * @param groupId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.AttributeValueCreateRequest} used by this operation.
     * @return {@code AttributeValue} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public AttributeValue createAttributeValue(Long groupId, VariantApiDtos.AttributeValueCreateRequest request) {
        AttributeGroup group = attributeGroupRepo.findById(groupId)
                .orElseThrow(() -> new IllegalArgumentException("Attribute group not found."));
        String code = normalizeCode(request == null ? null : request.code());
        String name = normalizeText(request == null ? null : request.displayName());
        if (code == null || name == null) {
            throw new IllegalArgumentException("Attribute value code and display name are required.");
        }
        if (attributeValueRepo.findByGroupAndCodeIgnoreCase(group, code).isPresent()) {
            throw new IllegalArgumentException("Attribute value code already exists in this group.");
        }
        AttributeValue value = new AttributeValue();
        value.setGroup(group);
        value.setCode(code);
        value.setDisplayName(name);
        value.setSortOrder(request != null && request.sortOrder() != null ? request.sortOrder() : 0);
        value.setActive(true);
        return attributeValueRepo.save(value);
    }

    /**
     * Executes the deleteAttributeGroup operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void deleteAttributeGroup(Long id) {
        attributeGroupRepo.deleteById(id);
    }

    /**
     * Executes the deleteAttributeValue operation.
     *
     * @param id Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void deleteAttributeValue(Long id) {
        attributeValueRepo.deleteById(id);
    }

    /**
     * Executes the configureProductAttributes operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.ProductAttributeConfigRequest} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void configureProductAttributes(Long productId, VariantApiDtos.ProductAttributeConfigRequest request) {
        Product product = requireProduct(productId);
        List<VariantApiDtos.ProductAttributeGroupSelection> groupSelections =
                request == null || request.groups() == null ? List.of() : request.groups();

        Map<Long, VariantApiDtos.ProductAttributeGroupSelection> selectedByGroupId = new LinkedHashMap<>();
        for (VariantApiDtos.ProductAttributeGroupSelection selection : groupSelections) {
            if (selection == null || selection.groupId() == null) continue;
            selectedByGroupId.put(selection.groupId(), selection);
        }

        if (selectedByGroupId.isEmpty()) {
            throw new IllegalArgumentException("At least one attribute group is required for configuration.");
        }

        List<AttributeGroup> groups = attributeGroupRepo.findAllById(selectedByGroupId.keySet());
        if (groups.size() != selectedByGroupId.size()) {
            throw new IllegalArgumentException("One or more attribute groups are invalid.");
        }
        Map<Long, AttributeGroup> groupMap = groups.stream().collect(Collectors.toMap(AttributeGroup::getId, g -> g));

        productAttributeValueRepo.deleteByProduct(product);
        productAttributeValueRepo.flush();
        productAttributeGroupRepo.deleteByProduct(product);
        productAttributeGroupRepo.flush();

        for (VariantApiDtos.ProductAttributeGroupSelection selection : selectedByGroupId.values()) {
            AttributeGroup group = groupMap.get(selection.groupId());
            ProductAttributeGroup mapping = new ProductAttributeGroup();
            mapping.setProduct(product);
            mapping.setGroup(group);
            mapping.setSortOrder(selection.sortOrder() == null ? 0 : selection.sortOrder());
            mapping.setRequired(selection.required() == null || selection.required());
            productAttributeGroupRepo.save(mapping);
        }

        List<VariantApiDtos.ProductAttributeAllowedValues> allowedValues =
                request == null || request.allowedValues() == null ? List.of() : request.allowedValues();
        Set<Long> allValueIds = new HashSet<>();
        for (VariantApiDtos.ProductAttributeAllowedValues allowed : allowedValues) {
            if (allowed == null || allowed.valueIds() == null) continue;
            allValueIds.addAll(allowed.valueIds());
        }
        Map<Long, AttributeValue> valueMap = attributeValueRepo.findByIdIn(allValueIds).stream()
                .collect(Collectors.toMap(AttributeValue::getId, v -> v));

        for (VariantApiDtos.ProductAttributeAllowedValues allowed : allowedValues) {
            if (allowed == null || allowed.groupId() == null || allowed.valueIds() == null) continue;
            AttributeGroup group = groupMap.get(allowed.groupId());
            if (group == null) {
                throw new IllegalArgumentException("Allowed value references a group not selected on product.");
            }
            for (Long valueId : allowed.valueIds()) {
                AttributeValue value = valueMap.get(valueId);
                if (value == null || value.getGroup() == null || !value.getGroup().getId().equals(group.getId())) {
                    throw new IllegalArgumentException("Value does not belong to selected attribute group.");
                }
                ProductAttributeValue pav = new ProductAttributeValue();
                pav.setProduct(product);
                pav.setGroup(group);
                pav.setValue(value);
                pav.setActive(true);
                productAttributeValueRepo.save(pav);
            }
        }
    }

    /**
     * Executes the generateVariants operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantGenerateRequest} used by this operation.
     * @return {@code VariantApiDtos.VariantGenerationResult} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public VariantApiDtos.VariantGenerationResult generateVariants(Long productId, VariantApiDtos.VariantGenerateRequest request) {
        Product product = requireProduct(productId);
        List<ProductAttributeGroup> groups = productAttributeGroupRepo.findByProductOrderBySortOrderAscIdAsc(product);
        if (groups.isEmpty()) {
            throw new IllegalArgumentException("No attribute groups configured for this product.");
        }

        List<ProductAttributeValue> allowedValues = productAttributeValueRepo.findByProductAndActiveTrueOrderByGroup_SortOrderAscValue_SortOrderAsc(product);
        Map<Long, List<AttributeValue>> valuesByGroupId = allowedValues.stream()
                .collect(Collectors.groupingBy(v -> v.getGroup().getId(), LinkedHashMap::new,
                        Collectors.mapping(ProductAttributeValue::getValue, Collectors.toList())));

        List<GroupValues> cartesianGroups = new ArrayList<>();
        for (ProductAttributeGroup group : groups) {
            List<AttributeValue> values = valuesByGroupId.getOrDefault(group.getGroup().getId(), List.of());
            if (Boolean.TRUE.equals(group.getRequired()) && values.isEmpty()) {
                throw new IllegalArgumentException("Required group has no active values: " + group.getGroup().getCode());
            }
            if (!values.isEmpty()) {
                cartesianGroups.add(new GroupValues(group.getGroup(), values));
            }
        }

        int expectedVariants = countCombinations(cartesianGroups);
        int maxVariants = request != null && request.maxVariants() != null ? request.maxVariants() : DEFAULT_MAX_VARIANTS;
        if (expectedVariants > maxVariants) {
            throw new IllegalArgumentException("Variant count exceeds maxVariants limit.");
        }

        Set<String> preserveFields = normalizePreserveFields(request == null ? null : request.preserveFields());
        VariantApiDtos.DefaultVariantValues defaults = request == null ? null : request.defaultValues();

        List<ProductVariantExclusion> exclusions = productVariantExclusionRepo.findByProductAndActiveTrue(product);
        Set<String> exclusionHashes = exclusions.stream().map(ProductVariantExclusion::getCombinationHash).collect(Collectors.toSet());

        List<ProductVariant> existingVariants = productVariantRepo.findByProductOrderByIdAsc(product);
        Map<String, ProductVariant> existingByHash = existingVariants.stream()
                .collect(Collectors.toMap(ProductVariant::getCombinationHash, v -> v, (a, b) -> a, LinkedHashMap::new));

        Counter counter = new Counter();
        Set<String> touchedHashes = new HashSet<>();
        buildCombinations(cartesianGroups, 0, new ArrayList<>(), combo -> {
            Map<String, String> kv = combo.stream().collect(Collectors.toMap(
                    c -> c.group().getCode(),
                    c -> c.value().getCode(),
                    (a, b) -> b,
                    LinkedHashMap::new
            ));
            String combinationKey = keyService.canonicalKey(kv);
            String combinationHash = keyService.hash(combinationKey);
            if (exclusionHashes.contains(combinationHash)) {
                counter.skippedExcluded++;
                return;
            }

            ProductVariant variant = existingByHash.get(combinationHash);
            if (variant == null) {
                variant = new ProductVariant();
                variant.setProduct(product);
                variant.setCombinationKey(combinationKey);
                variant.setCombinationHash(combinationHash);
                variant.setVariantName(toVariantName(combo));
                variant.setPrice(defaultPrice(product, defaults));
                variant.setCost(defaultCost(product, defaults));
                variant.setEnabled(defaultEnabled(defaults));
                variant.setImpossible(false);
                variant.setArchived(false);
                variant.setStockBaseQty(BigDecimal.ZERO);
                variant.setCreatedAt(LocalDateTime.now());
                variant.setUpdatedAt(LocalDateTime.now());
                variant = productVariantRepo.save(variant);
                syncVariantAttributes(variant, combo);
                counter.created++;
            } else {
                variant.setCombinationKey(combinationKey);
                variant.setVariantName(toVariantName(combo));
                variant.setArchived(false);
                applyNonPreservedDefaults(variant, preserveFields, defaults, product);
                variant.setUpdatedAt(LocalDateTime.now());
                variant = productVariantRepo.save(variant);
                syncVariantAttributes(variant, combo);
                counter.updated++;
            }
            touchedHashes.add(combinationHash);
        });

        for (ProductVariant existing : existingVariants) {
            if (!touchedHashes.contains(existing.getCombinationHash()) && !Boolean.TRUE.equals(existing.getArchived())) {
                existing.setArchived(true);
                existing.setEnabled(false);
                existing.setUpdatedAt(LocalDateTime.now());
                productVariantRepo.save(existing);
                counter.archived++;
            }
        }

        return new VariantApiDtos.VariantGenerationResult(
                product.getId(),
                expectedVariants,
                counter.created,
                counter.updated,
                counter.archived,
                counter.skippedExcluded
        );
    }

    /**
     * Executes the addExclusion operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantExclusionRequest} used by this operation.
     * @param authentication Parameter of type {@code Authentication} used by this operation.
     * @return {@code ProductVariantExclusion} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ProductVariantExclusion addExclusion(Long productId,
                                                VariantApiDtos.VariantExclusionRequest request,
                                                Authentication authentication) {
        Product product = requireProduct(productId);
        String rawKey = request == null ? null : request.combinationKey();
        Map<String, String> parsed = keyService.parseCanonicalKey(rawKey);
        String canonicalKey = keyService.canonicalKey(parsed);
        if (canonicalKey.isBlank()) {
            throw new IllegalArgumentException("combinationKey is required.");
        }
        String hash = keyService.hash(canonicalKey);

        ProductVariantExclusion exclusion = productVariantExclusionRepo
                .findByProductAndCombinationHash(product, hash)
                .orElseGet(ProductVariantExclusion::new);
        exclusion.setProduct(product);
        exclusion.setCombinationKey(canonicalKey);
        exclusion.setCombinationHash(hash);
        exclusion.setReason(request == null ? null : normalizeText(request.reason()));
        exclusion.setActive(true);
        exclusion.setCreatedBy(authentication == null ? "system" : authentication.getName());
        exclusion.setCreatedAt(LocalDateTime.now());
        ProductVariantExclusion saved = productVariantExclusionRepo.save(exclusion);

        productVariantRepo.findByProductAndCombinationHash(product, hash).ifPresent(variant -> {
            variant.setImpossible(true);
            variant.setEnabled(false);
            variant.setUpdatedAt(LocalDateTime.now());
            productVariantRepo.save(variant);
        });

        return saved;
    }

    /**
     * Executes the removeExclusion operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @param exclusionId Parameter of type {@code Long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public void removeExclusion(Long productId, Long exclusionId) {
        Product product = requireProduct(productId);
        ProductVariantExclusion exclusion = productVariantExclusionRepo.findById(exclusionId)
                .orElseThrow(() -> new IllegalArgumentException("Exclusion not found."));
        if (!exclusion.getProduct().getId().equals(product.getId())) {
            throw new IllegalArgumentException("Exclusion does not belong to product.");
        }
        exclusion.setActive(false);
        productVariantExclusionRepo.save(exclusion);
    }

    /**
     * Executes the updateVariantState operation.
     *
     * @param variantId Parameter of type {@code Long} used by this operation.
     * @param request Parameter of type {@code VariantApiDtos.VariantStateUpdateRequest} used by this operation.
     * @return {@code ProductVariant} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public ProductVariant updateVariantState(Long variantId, VariantApiDtos.VariantStateUpdateRequest request) {
        ProductVariant variant = productVariantRepo.findById(variantId)
                .orElseThrow(() -> new IllegalArgumentException("Variant not found."));
        if (request == null) return variant;

        if (request.impossible() != null) {
            variant.setImpossible(request.impossible());
            if (Boolean.TRUE.equals(request.impossible())) {
                variant.setEnabled(false);
            }
        }
        if (request.enabled() != null && !Boolean.TRUE.equals(variant.getImpossible())) {
            variant.setEnabled(request.enabled());
        }
        variant.setUpdatedAt(LocalDateTime.now());
        return productVariantRepo.save(variant);
    }

    /**
     * Executes the syncVariantAttributes operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @param combo Parameter of type {@code List<ComboValue>} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void syncVariantAttributes(ProductVariant variant, List<ComboValue> combo) {
        productVariantAttributeRepo.deleteByVariant(variant);
        productVariantAttributeRepo.flush();
        for (ComboValue item : combo) {
            ProductVariantAttribute pva = new ProductVariantAttribute();
            pva.setVariant(variant);
            pva.setGroup(item.group());
            pva.setValue(item.value());
            productVariantAttributeRepo.save(pva);
        }
    }

    /**
     * Executes the buildCombinations operation.
     *
     * @param groups Parameter of type {@code List<GroupValues>} used by this operation.
     * @param index Parameter of type {@code int} used by this operation.
     * @param current Parameter of type {@code List<ComboValue>} used by this operation.
     * @param consumer Parameter of type {@code java.util.function.Consumer<List<ComboValue>>} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void buildCombinations(List<GroupValues> groups,
                                   int index,
                                   List<ComboValue> current,
                                   java.util.function.Consumer<List<ComboValue>> consumer) {
        if (index >= groups.size()) {
            consumer.accept(List.copyOf(current));
            return;
        }
        GroupValues gv = groups.get(index);
        for (AttributeValue value : gv.values()) {
            current.add(new ComboValue(gv.group(), value));
            buildCombinations(groups, index + 1, current, consumer);
            current.remove(current.size() - 1);
        }
    }

    /**
     * Executes the countCombinations operation.
     *
     * @param groups Parameter of type {@code List<GroupValues>} used by this operation.
     * @return {@code int} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private int countCombinations(List<GroupValues> groups) {
        if (groups.isEmpty()) return 0;
        long total = 1L;
        for (GroupValues group : groups) {
            total *= Math.max(1, group.values().size());
            if (total > Integer.MAX_VALUE) {
                return Integer.MAX_VALUE;
            }
        }
        return (int) total;
    }

    /**
     * Executes the defaultPrice operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @param defaults Parameter of type {@code VariantApiDtos.DefaultVariantValues} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal defaultPrice(Product product, VariantApiDtos.DefaultVariantValues defaults) {
        if (defaults != null && defaults.price() != null) return defaults.price();
        return product.getPrice();
    }

    /**
     * Executes the defaultCost operation.
     *
     * @param product Parameter of type {@code Product} used by this operation.
     * @param defaults Parameter of type {@code VariantApiDtos.DefaultVariantValues} used by this operation.
     * @return {@code BigDecimal} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private BigDecimal defaultCost(Product product, VariantApiDtos.DefaultVariantValues defaults) {
        if (defaults != null && defaults.cost() != null) return defaults.cost();
        return product.getCostPrice();
    }

    /**
     * Executes the defaultEnabled operation.
     *
     * @param defaults Parameter of type {@code VariantApiDtos.DefaultVariantValues} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private boolean defaultEnabled(VariantApiDtos.DefaultVariantValues defaults) {
        return defaults == null || defaults.enabled() == null || defaults.enabled();
    }

    /**
     * Executes the applyNonPreservedDefaults operation.
     *
     * @param variant Parameter of type {@code ProductVariant} used by this operation.
     * @param preserve Parameter of type {@code Set<String>} used by this operation.
     * @param defaults Parameter of type {@code VariantApiDtos.DefaultVariantValues} used by this operation.
     * @param product Parameter of type {@code Product} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void applyNonPreservedDefaults(ProductVariant variant,
                                           Set<String> preserve,
                                           VariantApiDtos.DefaultVariantValues defaults,
                                           Product product) {
        if (!preserve.contains("price") && defaults != null && defaults.price() != null) {
            variant.setPrice(defaults.price());
        } else if (!preserve.contains("price") && defaults == null) {
            variant.setPrice(product.getPrice());
        }

        if (!preserve.contains("cost") && defaults != null && defaults.cost() != null) {
            variant.setCost(defaults.cost());
        } else if (!preserve.contains("cost") && defaults == null) {
            variant.setCost(product.getCostPrice());
        }

        if (!preserve.contains("enabled") && defaults != null && defaults.enabled() != null) {
            variant.setEnabled(defaults.enabled());
        }

        if (!preserve.contains("impossible")) {
            variant.setImpossible(false);
        }

        if (!preserve.contains("stock") && variant.getStockBaseQty() == null) {
            variant.setStockBaseQty(BigDecimal.ZERO);
        }
    }

    /**
     * Executes the normalizePreserveFields operation.
     *
     * @param fields Parameter of type {@code Collection<String>} used by this operation.
     * @return {@code Set<String>} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Set<String> normalizePreserveFields(Collection<String> fields) {
        if (fields == null || fields.isEmpty()) {
            return Set.of("sku", "barcode", "price", "cost", "stock", "enabled", "impossible");
        }
        return fields.stream()
                .filter(v -> v != null && !v.isBlank())
                .map(v -> v.trim().toLowerCase(Locale.ROOT))
                .collect(Collectors.toSet());
    }

    /**
     * Executes the toVariantName operation.
     *
     * @param combo Parameter of type {@code List<ComboValue>} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String toVariantName(List<ComboValue> combo) {
        return combo.stream()
                .map(v -> v.value().getDisplayName())
                .collect(Collectors.joining(" / "));
    }

    /**
     * Executes the requireProduct operation.
     *
     * @param productId Parameter of type {@code Long} used by this operation.
     * @return {@code Product} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Product requireProduct(Long productId) {
        return productRepo.findById(productId)
                .orElseThrow(() -> new IllegalArgumentException("Product not found."));
    }

    /**
     * Executes the normalizeCode operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeCode(String value) {
        if (value == null) return null;
        String normalized = value.trim().toUpperCase(Locale.ROOT);
        return normalized.isEmpty() ? null : normalized;
    }

    /**
     * Executes the normalizeText operation.
     *
     * @param value Parameter of type {@code String} used by this operation.
     * @return {@code String} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private String normalizeText(String value) {
        if (value == null) return null;
        String normalized = value.trim();
        return normalized.isEmpty() ? null : normalized;
    }

    private record GroupValues(AttributeGroup group, List<AttributeValue> values) {}

    private record ComboValue(AttributeGroup group, AttributeValue value) {}

    private static final class Counter {
        private int created;
        private int updated;
        private int archived;
        private int skippedExcluded;
    }
}
