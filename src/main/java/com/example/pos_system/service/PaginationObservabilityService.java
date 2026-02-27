package com.example.pos_system.service;

import org.springframework.stereotype.Service;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.List;
import java.util.concurrent.atomic.LongAdder;

@Service
public class PaginationObservabilityService {
    private static final int WINDOW_SIZE = 512;

    private final LongAdder totalRequests = new LongAdder();
    private final LongAdder errorRequests = new LongAdder();
    private final Deque<Long> responseLatencyMs = new ArrayDeque<>();
    private final Deque<Long> dbLatencyMs = new ArrayDeque<>();

    /**
     * Executes the recordSuccess operation.
     *
     * @param responseMs Parameter of type {@code long} used by this operation.
     * @param dbMs Parameter of type {@code long} used by this operation.
     * @return {@code Snapshot} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Snapshot recordSuccess(long responseMs, long dbMs) {
        totalRequests.increment();
        /**
         * Executes the synchronized operation.
         * <p>Return value: A fully initialized synchronized instance.</p>
         *
         * @param responseLatencyMs Parameter of type {@code Object} used by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        synchronized (responseLatencyMs) {
            append(responseLatencyMs, Math.max(0, responseMs));
            append(dbLatencyMs, Math.max(0, dbMs));
            return snapshotLocked();
        }
    }

    /**
     * Executes the recordError operation.
     *
     * @param responseMs Parameter of type {@code long} used by this operation.
     * @return {@code Snapshot} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Snapshot recordError(long responseMs) {
        totalRequests.increment();
        errorRequests.increment();
        /**
         * Executes the synchronized operation.
         * <p>Return value: A fully initialized synchronized instance.</p>
         *
         * @param responseLatencyMs Parameter of type {@code Object} used by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        synchronized (responseLatencyMs) {
            append(responseLatencyMs, Math.max(0, responseMs));
            return snapshotLocked();
        }
    }

    /**
     * Executes the snapshot operation.
     *
     * @return {@code Snapshot} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public Snapshot snapshot() {
        /**
         * Executes the synchronized operation.
         * <p>Return value: A fully initialized synchronized instance.</p>
         *
         * @param responseLatencyMs Parameter of type {@code Object} used by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        synchronized (responseLatencyMs) {
            return snapshotLocked();
        }
    }

    /**
     * Executes the append operation.
     *
     * @param bucket Parameter of type {@code Deque<Long>} used by this operation.
     * @param value Parameter of type {@code long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private void append(Deque<Long> bucket, long value) {
        bucket.addLast(value);
        while (bucket.size() > WINDOW_SIZE) {
            bucket.removeFirst();
        }
    }

    /**
     * Executes the snapshotLocked operation.
     *
     * @return {@code Snapshot} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private Snapshot snapshotLocked() {
        List<Long> response = new ArrayList<>(responseLatencyMs);
        List<Long> db = new ArrayList<>(dbLatencyMs);
        Collections.sort(response);
        Collections.sort(db);

        long total = Math.max(1, totalRequests.sum());
        double errorRate = (double) errorRequests.sum() / total;
        long p95Response = percentile(response, 0.95);
        long p95Db = percentile(db, 0.95);
        return new Snapshot(p95Response, p95Db, errorRate, totalRequests.sum(), errorRequests.sum());
    }

    /**
     * Executes the percentile operation.
     *
     * @param values Parameter of type {@code List<Long>} used by this operation.
     * @param pct Parameter of type {@code double} used by this operation.
     * @return {@code long} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    private long percentile(List<Long> values, double pct) {
        if (values == null || values.isEmpty()) return 0;
        int index = (int) Math.ceil(values.size() * pct) - 1;
        if (index < 0) index = 0;
        if (index >= values.size()) index = values.size() - 1;
        return values.get(index);
    }

    public record Snapshot(
            long p95ResponseMs,
            long p95DbMs,
            double errorRate,
            long totalRequests,
            long errorRequests
    ) {
    }
}
