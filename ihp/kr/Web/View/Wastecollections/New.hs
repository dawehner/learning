module Web.View.Wastecollections.New where
import Web.View.Prelude

data NewView = NewView { wastecollection :: Wastecollection }

instance View NewView ViewContext where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WastecollectionsAction}>Wastecollections</a></li>
                <li class="breadcrumb-item active">Edit Wastecollection</li>
            </ol>
        </nav>
        <h1>New Wastecollection</h1>
        {renderForm wastecollection}
    |]

renderForm :: Wastecollection -> Html
renderForm wastecollection = formFor wastecollection [hsx|
    {textField #title}
    {submitButton}
|]
