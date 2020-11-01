module Web.Types where
import IHP.Prelude
import qualified IHP.Controller.Session
import qualified IHP.ControllerSupport as ControllerSupport
import IHP.ModelSupport
import Application.Helper.Controller
import IHP.ViewSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [IHP.Controller.Session.FlashMessage]
    , controllerContext :: ControllerSupport.ControllerContext
    , layout :: Layout
    }

data WastecollectionsController
    = WastecollectionsAction
    | NewWastecollectionAction
    | ShowWastecollectionAction { wastecollectionId :: !(Id Wastecollection) }
    | CreateWastecollectionAction
    | EditWastecollectionAction { wastecollectionId :: !(Id Wastecollection) }
    | UpdateWastecollectionAction { wastecollectionId :: !(Id Wastecollection) }
    | DeleteWastecollectionAction { wastecollectionId :: !(Id Wastecollection) }
    deriving (Eq, Show, Data)
