module Web.View.Wastecollections.Edit where
import Web.View.Prelude

data EditView = EditView { wastecollection :: Wastecollection }

instance View EditView ViewContext where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WastecollectionsAction}>Wastecollections</a></li>
                <li class="breadcrumb-item active">Edit Wastecollection</li>
            </ol>
        </nav>
        <h1>Edit Wastecollection</h1>
        {renderForm wastecollection}
    |]

renderForm :: Wastecollection -> Html
renderForm wastecollection = formFor wastecollection [hsx|
    {textField #title}
    {submitButton}
|]
