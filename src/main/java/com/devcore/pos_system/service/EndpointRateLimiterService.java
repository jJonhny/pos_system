package com.devcore.pos_system.service;

import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

@Service
public class EndpointRateLimiterService {
    private final Map<String, WindowCounter> counters = new ConcurrentHashMap<>();

    /**
     * Executes the allow operation.
     *
     * @param key Parameter of type {@code String} used by this operation.
     * @param limit Parameter of type {@code int} used by this operation.
     * @param window Parameter of type {@code Duration} used by this operation.
     * @return {@code boolean} Result produced by this operation.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
    public boolean allow(String key, int limit, Duration window) {
        if (key == null || key.isBlank() || limit <= 0 || window == null || window.isZero() || window.isNegative()) {
            return false;
        }
        long now = System.currentTimeMillis();
        long windowMs = window.toMillis();
        WindowCounter counter = counters.computeIfAbsent(key, ignored -> new WindowCounter(now));
        /**
         * Executes the synchronized operation.
         * <p>Return value: A fully initialized synchronized instance.</p>
         *
         * @param counter Parameter of type {@code Object} used by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
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

    /**
     * Executes the cleanup operation.
     *
     * @param now Parameter of type {@code long} used by this operation.
     * @param windowMs Parameter of type {@code long} used by this operation.
     * @return void No value is returned; the method applies side effects to existing state.
     * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
     * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
     */
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

        /**
         * Executes the WindowCounter operation.
         * <p>Return value: A fully initialized WindowCounter instance.</p>
         *
         * @param now Parameter of type {@code long} used by this operation.
         * <p>Possible exceptions: Runtime exceptions from downstream dependencies may propagate unchanged.</p>
         * <p>Edge cases: Null, empty, and boundary inputs are handled by the existing control flow and validations.</p>
         */
        private WindowCounter(long now) {
            this.windowStartMs = now;
            this.lastSeenMs = now;
        }
    }
}
