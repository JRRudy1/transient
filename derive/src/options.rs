use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{Path, Token};

use crate::{Error, Result};

enum TransientOption {
    Crate(Path),
}

impl Parse for TransientOption {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![crate]) {
            let _: Token![crate] = input.parse()?;
            let _: Token![=] = input.parse()?;
            input.parse().map(Self::Crate)
        } else {
            Err(Error::InvalidOption(input.span()).into())
        }
    }
}

pub(crate) struct TransientOptions {
    pub(crate) krate: syn::Path,
}

impl TransientOptions {
    fn set(&mut self, opt: TransientOption) {
        match opt {
            TransientOption::Crate(path) => {
                self.krate = path;
            }
        }
    }
}

impl Default for TransientOptions {
    fn default() -> Self {
        TransientOptions { krate: syn::parse_quote! { ::transient } }
    }
}

impl TransientOptions {
    pub(crate) fn extract(attrs: &[syn::Attribute]) -> Result<Self> {
        let mut options = Self::default();
        let parser = Punctuated::<TransientOption, syn::Token![,]>::parse_terminated;
        for attr in attrs
            .iter()
            .filter(|attr| attr.path().is_ident("transient"))
        {
            for option in attr.parse_args_with(parser)? {
                options.set(option)
            }
        }
        Ok(options)
    }
}
