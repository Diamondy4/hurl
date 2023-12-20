{-# LANGUAGE TemplateHaskell #-}

module Internal.Options.TH where

import Control.Monad
import Data.Singletons
import Foreign.C
import Foreign.Ptr
import Internal.Options.Class
import Internal.Raw.CurlFunctions
import Language.Haskell.TH

genEasyOptionEnumInstances :: [(Name, Name)] -> Q [Dec]
genEasyOptionEnumInstances names = join <$> traverse genEasyOptionEnumInstance names

genEasyOptionEnumInstance :: (Name, Name) -> Q [Dec]
genEasyOptionEnumInstance (enumName, instanceType) =
    [d|
        instance EasyOption $instanceType' where
            type CurlParamBaseType $instanceType' = $enumName'
            setEasyOption easyPtr opt = do
                let longVal = fromIntegral . fromEnum $ opt
                let optVal = fromIntegral . fromEnum $ demote @($instanceType')
                curl_easy_setopt_long easyPtr optVal longVal
            {-# INLINE setEasyOption #-}
        |]
  where
    instanceType' = pure (ConT instanceType)
    enumName' = pure (ConT enumName)

genEasyOptionBoolInstances :: [Name] -> Q [Dec]
genEasyOptionBoolInstances names = join <$> traverse genEasyOptionEnumInstance ((''Bool,) <$> names)

genEasyOptionLongInstances :: [Name] -> Q [Dec]
genEasyOptionLongInstances names = join <$> traverse genEasyOptionLongInstance names

genEasyOptionLongInstance :: Name -> Q [Dec]
genEasyOptionLongInstance instanceType =
    [d|
        instance EasyOption $instanceType' where
            type CurlParamBaseType $instanceType' = Int
            setEasyOption easyPtr opt = do
                let longVal = fromIntegral opt
                let optVal = fromIntegral . fromEnum $ demote @($instanceType')
                curl_easy_setopt_long easyPtr optVal longVal
            {-# INLINE setEasyOption #-}
        |]
  where
    instanceType' = pure (ConT instanceType)

genEasyOptionStringInstances :: [Name] -> Q [Dec]
genEasyOptionStringInstances names = join <$> traverse genEasyOptionStringInstance names

genEasyOptionStringInstance :: Name -> Q [Dec]
genEasyOptionStringInstance instanceType =
    [d|
        instance EasyOption $instanceType' where
            type CurlParamBaseType $instanceType' = Ptr CChar
            setEasyOption easyPtr str = do
                let optVal = fromIntegral . fromEnum $ demote @($instanceType')
                curl_easy_setopt_string easyPtr optVal str
            {-# INLINE setEasyOption #-}
        |]
  where
    instanceType' = pure (ConT instanceType)
