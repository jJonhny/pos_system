package com.example.pos_system.service;

import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

@Service
public class EndpointRateLimiterService {
    private final Map<String, WindowCounter> counters = new ConcurrentHashMap<>();

    public boolean allow(String key, int limit, Duration window) {
        if (key == null || key.isBlank() || limit <= 0 || window == null || window.isZero() || window.isNegative()) {
            return false;
        }
        long now = System.currentTimeMillis();
        long windowMs = window.toMillis();
        WindowCounter counter = counters.computeIfAbsent(key, ignored -> new WindowCounter(now));
        synchronized (counter) {
            if (now - counter.windowStartMs >= windowMs) {
                counter.windowStartMs = now;
                counter.count.set(0);
            }
            int current = counter.count.incrementAndGet();
            counter.lastSeenMs = now;
            if (current > limit) {
                return false;
            }
        }

        cleanup(now, windowMs);
        return true;
    }

    private void cleanup(long now, long windowMs) {
        if (counters.size() < 2_000) return;
        Iterator<Map.Entry<String, WindowCounter>> iterator = counters.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<String, WindowCounter> entry = iterator.next();
            WindowCounter counter = entry.getValue();
            if (now - counter.lastSeenMs > windowMs * 3) {
                iterator.remove();
            }
        }
    }

    private static final class WindowCounter {
        private final AtomicInteger count = new AtomicInteger(0);
        private volatile long windowStartMs;
        private volatile long lastSeenMs;

        private WindowCounter(long now) {
            this.windowStartMs = now;
            this.lastSeenMs = now;
        }
    }
}
