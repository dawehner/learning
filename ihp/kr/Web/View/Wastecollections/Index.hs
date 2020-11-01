module Web.View.Wastecollections.Index where
import Web.View.Prelude

data IndexView = IndexView { wastecollections :: [Wastecollection] }

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={WastecollectionsAction}>Wastecollections</a></li>
            </ol>
        </nav>
        <h1>Wastecollections <a href={pathTo NewWastecollectionAction} class="btn btn-primary ml-4">+ New</a></h1>
        <table class="table table-responsive">
            <thead>
                <tr>
                    <th>Wastecollection</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>{forEach wastecollections renderWastecollection}</tbody>
        </table>
    |]


renderWastecollection wastecollection = [hsx|
    <tr>
        <td>{wastecollection}</td>
        <td><a href={ShowWastecollectionAction (get #id wastecollection)}>Show</a></td>
        <td><a href={EditWastecollectionAction (get #id wastecollection)} class="text-muted">edit</a></td>
        <td><a href={DeleteWastecollectionAction (get #id wastecollection)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
