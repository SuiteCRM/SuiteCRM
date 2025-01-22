<?php

use Api\Core\Loader\CustomLoader;

return CustomLoader::mergeCustomArray([
    'settings' => [
        /** Additional information about exceptions are displayed by the default error handler. */
        'displayErrorDetails' => true,
        /** Routes are accessible in middleware. */
        'determineRouteBeforeAppMiddleware' => true,
        'addContentLengthHeader' => false,
    ]
], basename(__FILE__));
