# Ollama Embedding Service (k8s)

CPU-only Ollama deployment running `nomic-embed-text` for hive embedding operations.

## Deploy

```bash
kubectl apply -k k8s/ollama-embed/
```

## Configure

Set the embeddings host in hive-mcp config:

```clojure
{:embeddings {:ollama {:host "http://ollama-embed.hive.svc.cluster.local:11434"}}}
```

## Verify

```bash
# Check pod status
kubectl get pods -n hive -l app=ollama-embed

# Verify model is loaded
kubectl exec -it -n hive deploy/ollama-embed -- ollama list

# Test embedding endpoint
kubectl exec -it -n hive deploy/ollama-embed -- \
  curl -s http://localhost:11434/api/embeddings \
    -d '{"model":"nomic-embed-text","prompt":"test"}'
```

## Notes

- **CPU-only**: No GPU resources requested. Suitable for light embedding workloads.
- **Model preload**: Init container pulls `nomic-embed-text` before the main container starts.
- **Storage**: Uses `emptyDir` volume — model persists across container restarts within the pod, but is lost on pod deletion/rescheduling.
- **Internal only**: ClusterIP service, not exposed externally. DNS: `ollama-embed.hive.svc.cluster.local`
