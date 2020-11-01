module Web.View.Wastecollections.Show where
import Web.View.Prelude

data ShowView = ShowView { wastecollection :: Wastecollection }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WastecollectionsAction}>Wastecollections</a></li>
                <li class="breadcrumb-item active">Show Wastecollection</li>
            </ol>
        </nav>
        <h1>Show Wastecollection</h1>
    |]
