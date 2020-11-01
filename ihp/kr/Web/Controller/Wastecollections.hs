module Web.Controller.Wastecollections where

import Web.Controller.Prelude
import Web.View.Wastecollections.Index
import Web.View.Wastecollections.New
import Web.View.Wastecollections.Edit
import Web.View.Wastecollections.Show

instance Controller WastecollectionsController where
    action WastecollectionsAction = do
        wastecollections <- query @Wastecollection |> fetch
        render IndexView { .. }

    action NewWastecollectionAction = do
        let wastecollection = newRecord
        render NewView { .. }

    action ShowWastecollectionAction { wastecollectionId } = do
        wastecollection <- fetch wastecollectionId
        render ShowView { .. }

    action EditWastecollectionAction { wastecollectionId } = do
        wastecollection <- fetch wastecollectionId
        render EditView { .. }

    action UpdateWastecollectionAction { wastecollectionId } = do
        wastecollection <- fetch wastecollectionId
        wastecollection
            |> buildWastecollection
            |> ifValid \case
                Left wastecollection -> render EditView { .. }
                Right wastecollection -> do
                    wastecollection <- wastecollection |> updateRecord
                    setSuccessMessage "Wastecollection updated"
                    redirectTo EditWastecollectionAction { .. }

    action CreateWastecollectionAction = do
        let wastecollection = newRecord @Wastecollection
        wastecollection
            |> buildWastecollection
            |> ifValid \case
                Left wastecollection -> render NewView { .. } 
                Right wastecollection -> do
                    wastecollection <- wastecollection |> createRecord
                    setSuccessMessage "Wastecollection created"
                    redirectTo WastecollectionsAction

    action DeleteWastecollectionAction { wastecollectionId } = do
        wastecollection <- fetch wastecollectionId
        deleteRecord wastecollection
        setSuccessMessage "Wastecollection deleted"
        redirectTo WastecollectionsAction

buildWastecollection wastecollection = wastecollection
    |> fill @'["title"]
