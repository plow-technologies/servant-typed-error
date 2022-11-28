# servant-typed-error

This is a small wrapper for the exception approach detailed here: https://docs.servant.dev/en/stable/cookbook/uverb/UVerb.html?highlight=exceptions

This library allows sending and receiving Typed errors via servant. For example, we can define the following API:

```haskell
data APIError = Whoops | Bad Int
  deriving (Generic, ToJSON, FromJSON)

type API =
  "foo" :> Capture "number" Int :> GetTypedError '[JSON] Bool APIError
    :<|> "whoops" :> GetTypedError '[JSON] Int APIError
```

Here we use `GetTypedError` instead of the usual `Get` to indicate the API can return an `ApiError`, which must have a `ToJSON`/`FromJSON` instance.

There are two ways of writing a server for this API. We can either use the `TypedHandler` monad, which has `throwTypedError` and `throwServantError` functions allowing us to either throw our custom typed error or a generic `ServantError`. Another approach using a generic `mtl` style definition of a monad with a `MonadError e m` constraint; we can then use `liftTypedError` to lift it into servant's `Handler` monad:


```haskell
alwaysWhoops :: MonadError APIError m => m Int
alwaysWhoops = throwError Whoops

server :: Server API
server =
  ( \i ->
      runTypedHandler $ case i of
        42 -> throwTypedError $ Bad i
        -1 -> throwServantError err500
        x -> pure $ x `mod` 2 == 0
  )
    :<|> liftTypedError alwaysWhoops
```

Finally, we can recover the errors on the client side via a special `TypedClientM e a` monad. We use the `typedClient` to convert the servant-client API to use our `TypedClientM`:

```haskell
foo :: Int -> TypedClientM APIError Bool
whoops :: TypedClientM APIError Int
foo :<|> whoops = typedClient $ client $ Proxy @API
```

Then, using `runTypedClientM` we can obtain an `Either (Either ClientError APIError) a` and pattern match on both the generic servant-client error or the user defined `APIError`.


## Differences with other servant typed error/exception libs

- [servant-checked-exceptions](https://hackage.haskell.org/package/servant-checked-exceptions) This library wraps the response in a custom type, so it would be a bit awkward to use with the frontend. The approach here sends the same 200 response as vanilla servant would

- [servant-exceptions](https://github.com/ch1bo/servant-exceptions) Not investigated much since it's missing the client implementation