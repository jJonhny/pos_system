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

    public Snapshot recordSuccess(long responseMs, long dbMs) {
        totalRequests.increment();
        synchronized (responseLatencyMs) {
            append(responseLatencyMs, Math.max(0, responseMs));
            append(dbLatencyMs, Math.max(0, dbMs));
            return snapshotLocked();
        }
    }

    public Snapshot recordError(long responseMs) {
        totalRequests.increment();
        errorRequests.increment();
        synchronized (responseLatencyMs) {
            append(responseLatencyMs, Math.max(0, responseMs));
            return snapshotLocked();
        }
    }

    public Snapshot snapshot() {
        synchronized (responseLatencyMs) {
            return snapshotLocked();
        }
    }

    private void append(Deque<Long> bucket, long value) {
        bucket.addLast(value);
        while (bucket.size() > WINDOW_SIZE) {
            bucket.removeFirst();
        }
    }

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
